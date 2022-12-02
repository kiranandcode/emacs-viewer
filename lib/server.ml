open Core
module Sexp = Sexplib.Sexp

let log = lazy (Dream.sub_log "server")

let emacs = Bos.Cmd.v "emacsclient.emacs"
let emacs_cmd fmt =
  let open Bos in
  Format.ksprintf (fun s -> 
    OS.Cmd.run_out Cmd.(emacs % "-e" % s)
    |> OS.Cmd.to_string
    |> Result.map ~f:Sexplib.Sexp.of_string
    |> Result.map_error ~f:(fun _ -> `Msg "emacs RPC failed")
  ) fmt

let emacs_cmd_unit fmt =
  let open Bos in
  Format.ksprintf (fun s -> 
    OS.Cmd.run Cmd.(emacs % "-e" % s)
    |> Result.map_error ~f:(fun _ -> `Msg "emacs RPC failed")
  ) fmt


let html ?status ?code ?headers s = Lwt_result.ok @@ Dream.html ?status ?code ?headers s
let sexp ?status ?code ?headers s =
  let headers = Option.value ~default:[] headers
                |> List.cons ("Content-Type", "text/plain") in
  Lwt_result.ok @@
  Dream.respond ?status ?code ~headers (Sexp.to_string_mach s)

let lift_decoders_error res =
  Lwt_result.lift (
    Result.map_error ~f:(fun e -> `Msg (Decoders.Error.to_string Sexplib.Sexp.pp_hum e))
      res
  )

let handle_error v = Lwt.bind v (function
  | Ok v -> Lwt.return v
  | Error (`Msg err) -> Dream.respond ~status:`Internal_Server_Error err)

module State = struct
  type buffer_data = {
    modification_time: Emacs_data.Data.buffer_timestamp;
    buffer_name: string;
    buffer_filename: string;
    buffer_data: Emacs_data.Data.t list
  } [@@deriving sexp, eq, show]

  type t = (string, buffer_data option) Hashtbl.t

  let get_buffer_data (t: t) buffer_filename modification_time =
    match Hashtbl.find t buffer_filename with
    | Some (Some data as cached_data) when
        Emacs_data.Data.equal_buffer_timestamp
          data.modification_time modification_time ->
      cached_data
    | _ -> None

  let set_buffer_data (t: t) buffer_filename buffer_data =
    Hashtbl.update_and_return t buffer_filename ~f:(function
      | Some (Some cached_data) when
          Emacs_data.Data.buffer_timestamp_gt
            buffer_data.modification_time
            cached_data.modification_time ->
        (Some cached_data)
      | _ -> Some buffer_data
    )
    |> Option.value_exn ~here:[%here] ~message:"option should always be non-None here"


  let set_buffer_list (t: t) ls =
    let ls = Hashtbl.of_alist_exn (module String) ls in
    Hashtbl.filter_mapi_inplace t ~f:(fun ~key ~data ->
      match Hashtbl.find ls key with
      | None ->
        (let lazy log = log in log).info (fun f -> f "dropping cached buffer %s" key);
        (* buffer is no longer present in emacs; drop it from the server *)
        None
      (* buffer exists *)
      | Some (_, timestamp) ->
        (* remove from ls *)
        Hashtbl.remove ls key;
        (* cached data *)
        match data with
        | Some old_data when
            Emacs_data.Data.equal_buffer_timestamp
              old_data.modification_time timestamp ->
          (* if we have the latest modification time, then keep cached *)
          Some data
        | _ ->
          (* otherwise, clear cached, but keep entry *)
          Some None
    );
    Hashtbl.iteri ls ~f:(fun ~key:fname ~data:_ ->
      Hashtbl.add_exn t ~key:fname ~data:None
    )

  let buffer_list (t: t) =
    let res = ref [] in
    Hashtbl.iteri t ~f:(fun ~key ~data ->
      let buffer_name = Option.map ~f:(fun data -> data.buffer_name) data in
      res := (key,buffer_name) :: !res);
    List.rev !res

end

let state : State.t = Hashtbl.create (module String)
let state_lock = Lwt_mutex.create ()

let get_buffer_list () =
  let open Lwt_result.Let_syntax in
  let%bind sxp = Lwt_result.lift @@ emacs_cmd {elisp|
(progn
  (let ((elements nil))
    (dolist (buffer (org-buffer-list))
      (with-current-buffer buffer
        (let ((visit-time (time-convert (visited-file-modtime) 'integer))
              (mod-time (buffer-modified-tick)))
          (push (list (buffer-file-name) (buffer-name) visit-time mod-time) elements))
        )
      )
    elements))
|elisp} in
  let%bind data = lift_decoders_error (Org_data.buffer_list sxp) in
  Lwt_result.return data

let get_buffer_timestamp buffer_name =
  let open Lwt_result.Let_syntax in
  let%bind sxp = Lwt_result.lift @@ emacs_cmd {elisp|
(let ((buffer (find-buffer-visiting "%s")))
  (with-current-buffer buffer
    (let ((visit-time (time-convert (visited-file-modtime) 'integer))
          (mod-time (buffer-modified-tick)))
      (list visit-time mod-time))))
|elisp} buffer_name in
  let%bind data = lift_decoders_error (Org_data.buffer_timestamp sxp) in
  Lwt_result.return data

let get_buffer_data buffer_filename =
  let open Lwt_result.Let_syntax in
  let%bind sxp = Lwt_result.lift @@ emacs_cmd {elisp|
(let ((buffer (find-buffer-visiting "%s")))
  (with-current-buffer buffer
    (let ((visit-time (time-convert (visited-file-modtime) 'integer))
          (mod-time (buffer-modified-tick)))
      (list (buffer-name)
            (list visit-time mod-time)
            (org-element-parse-buffer)))))
|elisp} buffer_filename in
  let%bind (buffer_name, modification_time, buffer_data) =
    lift_decoders_error (Org_data.buffer_data sxp) in
  let data = State.{
    modification_time;
    buffer_name;
    buffer_filename;
    buffer_data
  } in
  Lwt_result.return data

let run_at_pos ~filename:buffer_filename ~pos ~focus command =
  let open Lwt_result.Let_syntax in
  let cmd =
    if focus
    then
      Format.sprintf {elisp|
(let ((buffer (find-buffer-visiting "%s")))
  (select-window (or (get-buffer-window buffer) (selected-window)))
  (switch-to-buffer buffer)
  (goto-char %d)
  (%s))
|elisp} buffer_filename pos command
    else
      Format.sprintf {elisp|
(let ((buffer (find-buffer-visiting "%s")))
  (with-current-buffer buffer
        (goto-char %d)
        (%s)))
|elisp} buffer_filename pos command in
  let%bind () = Lwt_result.lift @@ emacs_cmd_unit "%s" cmd in
  Lwt_result.return ()

let api_routes =
  let open Lwt_result.Let_syntax in
  Dream.scope "/api" [] [
    Dream.get "/buffers" (fun _ ->
      (let lazy log = log in log).info (fun f -> f "buffers");
      handle_error begin
        let%bind data = get_buffer_list () in
        let%bind buffer_list = 
          Lwt_mutex.with_lock state_lock (fun () ->
            State.set_buffer_list state data;
            Lwt_result.return (State.buffer_list state)
          ) in
        sexp ([%sexp_of: (string * string option) list] buffer_list)
      end
    );
    Dream.post "/buffer/load" (fun req ->
      handle_error begin
        let%bind buffer_filename = Lwt_result.ok @@ Dream.body req in
        let%bind timestamp = get_buffer_timestamp buffer_filename
                             |> Lwt_result.map_error
                                  (fun _ -> `Msg "Emacs RPC failed - invalid filename?") in
        let%bind cached_data = Lwt_mutex.with_lock state_lock (fun () ->
          Lwt_result.return @@ State.get_buffer_data state buffer_filename timestamp
        ) in
        let%bind data = match cached_data with
          | Some data -> Lwt_result.return data
          | None ->
            let%bind data = get_buffer_data buffer_filename in
            Lwt_mutex.with_lock state_lock (fun () ->
              Lwt_result.return @@ State.set_buffer_data state buffer_filename data;
            ) in
        sexp ([%sexp_of:
               Emacs_data.Data.t list *
               Emacs_data.Data.buffer_timestamp]
                (data.buffer_data, data.modification_time))
      end
    );
    Dream.post "/buffer/reload" (fun req ->
      handle_error begin
        let%bind buffer_data = Lwt_result.ok @@ Dream.body req in
        let%bind buffer_filename, client_timestamp =
          Result.try_with (fun () -> [%of_sexp: (string * Emacs_data.Data.buffer_timestamp)] (Sexp.of_string buffer_data))
          |> Result.map_error ~f:(fun err -> `Msg ("Invalid data: " ^ Exn.to_string err))
          |> Lwt.return in
        let%bind last_modified_timestamp = get_buffer_timestamp buffer_filename
                                           |> Lwt_result.map_error
                                                (fun _ -> `Msg "Emacs RPC failed - invalid filename?") in
        if Emacs_data.Data.buffer_timestamp_gt client_timestamp last_modified_timestamp
        then begin
          let%bind cached_data = Lwt_mutex.with_lock state_lock (fun () ->
            Lwt_result.return @@ State.get_buffer_data state buffer_filename last_modified_timestamp
          ) in
          let%bind data = match cached_data with
            | Some data -> Lwt_result.return data
            | None ->
              let%bind data = get_buffer_data buffer_filename in
              Lwt_mutex.with_lock state_lock (fun () ->
                Lwt_result.return @@ State.set_buffer_data state buffer_filename data;
              ) in
          sexp ([%sexp_of: (Emacs_data.Data.t list * Emacs_data.Data.buffer_timestamp) option]
                  (Some (data.buffer_data, data.modification_time)))
        end
        else sexp ([%sexp_of: (Emacs_data.Data.t list * Emacs_data.Data.buffer_timestamp) option]
                     None)
      end
    );    
    Dream.post "/buffer/action" (fun req ->
      handle_error begin
        let%bind request_data = Lwt_result.ok @@ Dream.body req in
        let%bind buffer_filename, client_timestamp, pos, action =
          Result.try_with (fun () ->
            [%of_sexp: (string * Emacs_data.Data.buffer_timestamp * int *
                        [`clock_in | `clock_out | `change_todo | `open_in_emacs ])]
              (Sexp.of_string request_data))
          |> Result.map_error ~f:(fun err -> `Msg ("Invalid data: " ^ Exn.to_string err))
          |> Lwt.return in
        let%bind last_modified_timestamp =
          get_buffer_timestamp buffer_filename
          |> Lwt_result.map_error
               (fun _ -> `Msg "Emacs RPC failed - invalid filename?") in
        if Emacs_data.Data.buffer_timestamp_gt client_timestamp last_modified_timestamp
        then sexp ([%sexp_of: unit option] None)
        else begin
          let command, focus = match action with
            | `clock_in -> "org-clock-in", false
            | `clock_out -> "org-clock-out", false
            | `change_todo -> "org-todo", false
            | `open_in_emacs -> "progn", true in
          let%bind () = run_at_pos ~filename:buffer_filename ~pos ~focus command in
          sexp ([%sexp_of: unit option] (Some ()))
        end
      end
    )
  ]

let run port =
  (* Dream.initialize_log ~enable:true ~level:`Debug (); *)
  Dream.run ?port begin
    Dream.logger @@
    Dream.router [
      api_routes;

      Dream.scope "/static" [] [
        Dream.get "/js/**" @@ Dream.static "_build/default/js";
        Dream.get "/styles/**" @@ Dream.static "_build/default/styles";
      ];

      Dream.get "/" (fun _req ->
        Dream.html {html|
          <!doctype html>
          <html lang="en">
             <head>
                 <meta charset="utf-8">
                 <meta name="viewport" content="width=device-width, initial-scale=1">
<title>Emacs Org Viewer</title>
   <link rel="stylesheet" href="static/styles/style.css">
             </head>
             <body>
                <div id="app">
                </div>
                <script src="static/js/app.bc.js"></script>
             </body>
          </html>
        |html}
      );
    ]
  end
