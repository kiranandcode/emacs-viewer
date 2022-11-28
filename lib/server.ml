open Core
module Sexp = Sexplib.Sexp

module Log = (val (Logs.src_log (Logs.Src.create "server")))

let emacs = Bos.Cmd.v "emacsclient.emacs"
let emacs_cmd fmt =
  let open Bos in
  Format.ksprintf (fun s -> 
    OS.Cmd.run_out Cmd.(emacs % "-e" % s)
    |> OS.Cmd.to_string
    |> Result.map ~f:Sexplib.Sexp.of_string) fmt


let collect_todos_cmd = {elisp|
(progn
  (let ((elements nil))
    (dolist (buffer (org-buffer-list))
      (with-current-buffer buffer
        (push (cons (buffer-name) (org-element-parse-buffer)) elements)
        )
      )
    elements))
|elisp}

let html ?status ?code ?headers s = Lwt_result.ok @@ Dream.html ?status ?code ?headers s
let sexp ?status ?code ?headers s = Lwt_result.ok @@ Dream.respond ?status ?code ?headers (Sexp.to_string_mach s)

let lift_decoders_error res =
  Lwt_result.lift (
    Result.map_error ~f:(fun e -> `Msg (Decoders.Error.to_string Sexplib.Sexp.pp_hum e))
      res
  )

let handle_error v = Lwt.bind v (function
  | Ok v -> Lwt.return v
  | Error (`Msg err) -> Dream.respond ~status:`Internal_Server_Error err)

let state: (string, unit) Base.Hashtbl.t = Hashtbl.create (module String)

module State = struct
  type buffer_data = {
    modification_time: Emacs_data.Data.buffer_timestamp;
    buffer_name: string;
    buffer_filename: string;
    buffer_data: Emacs_data.Data.t list
  } [@@deriving sexp, eq, show]

  type t = (string, buffer_data option) Hashtbl.t

  let set_buffer_list (t: t) ls =
    let ls = Hashtbl.of_alist_exn (module String) ls in
    Hashtbl.filter_mapi_inplace t ~f:(fun ~key ~data ->
      match Hashtbl.find ls key with
      | None ->
        Log.debug (fun f -> f "dropping cached buffer %s" key);
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

let get_buffer_data buffer_name =
  let open Lwt_result.Let_syntax in
  let%bind _sxp = Lwt_result.lift @@ emacs_cmd {elisp|
(let ((buffer (find-buffer-visiting "%s")))
  (with-current-buffer buffer
    (let ((visit-time (time-convert (visited-file-modtime) 'integer))
          (mod-time (buffer-modified-tick)))
      (list (buffer-name)
            (list visit-time mod-time)
            (org-element-parse-buffer)))))
|elisp} buffer_name in
  Lwt_result.return ()

let api_routes =
  let open Lwt_result.Let_syntax in
  Dream.scope "/api" [] [
    Dream.get "/buffers" (fun _ ->
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
    Dream.post "/buffer" (fun req ->
      handle_error begin
        let%bind body = Lwt_result.ok @@ Dream.body req in
        sexp ([%sexp_of: string] body)
      end
    )
  ]

let run port =
  Dream.run ?port begin
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
