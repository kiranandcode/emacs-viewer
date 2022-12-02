open Core
module Sexp = Sexplib.Sexp

let log = lazy (Dream.sub_log "server")

let html ?status ?code ?headers s = Lwt_result.ok @@ Dream.html ?status ?code ?headers s
let sexp ?status ?code ?headers s =
  let headers = Option.value ~default:[] headers
                |> List.cons ("Content-Type", "text/plain") in
  Lwt_result.ok @@
  Dream.respond ?status ?code ~headers (Sexp.to_string_mach s)

let handle_error v = Lwt.bind v (function
  | Ok v -> Lwt.return v
  | Error (`Msg err) -> Dream.respond ~status:`Internal_Server_Error err)

let state : State.t = State.create ()
let state_lock = Lwt_mutex.create ()

let api_routes =
  let open Lwt_result.Let_syntax in
  Dream.scope "/api" [] [
    Dream.get "/buffers" (fun _ ->
      (let lazy log = log in log).info (fun f -> f "buffers");
      handle_error begin
        let%bind data = Emacs.get_buffer_list () in
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
        let%bind timestamp = Emacs.get_buffer_timestamp buffer_filename
                             |> Lwt_result.map_error
                                  (fun _ -> `Msg "Emacs RPC failed - invalid filename?") in
        let%bind cached_data = Lwt_mutex.with_lock state_lock (fun () ->
          Lwt_result.return @@ State.get_buffer_data state buffer_filename timestamp
        ) in
        let%bind data = match cached_data with
          | Some data -> Lwt_result.return data
          | None ->
            let%bind data = Emacs.get_buffer_data buffer_filename in
            Lwt_mutex.with_lock state_lock (fun () ->
              Lwt_result.return @@ State.set_buffer_data state buffer_filename data;
            ) in
        sexp ([%sexp_of:
               Emacs_data.t list *
               Emacs_data.buffer_timestamp]
                (data.buffer_data, data.modification_time))
      end
    );
    Dream.post "/buffer/reload" (fun req ->
      handle_error begin
        let%bind buffer_data = Lwt_result.ok @@ Dream.body req in
        let%bind buffer_filename, client_timestamp =
          Result.try_with (fun () -> [%of_sexp: (string * Emacs_data.buffer_timestamp)] (Sexp.of_string buffer_data))
          |> Result.map_error ~f:(fun err -> `Msg ("Invalid data: " ^ Exn.to_string err))
          |> Lwt.return in
        let%bind last_modified_timestamp =
          Emacs.get_buffer_timestamp buffer_filename
          |> Lwt_result.map_error
               (fun _ -> `Msg "Emacs RPC failed - invalid filename?") in
        if Emacs_data.buffer_timestamp_gt client_timestamp last_modified_timestamp
        then begin
          let%bind cached_data = Lwt_mutex.with_lock state_lock (fun () ->
            Lwt_result.return @@ State.get_buffer_data state buffer_filename last_modified_timestamp
          ) in
          let%bind data = match cached_data with
            | Some data -> Lwt_result.return data
            | None ->
              let%bind data = Emacs.get_buffer_data buffer_filename in
              Lwt_mutex.with_lock state_lock (fun () ->
                Lwt_result.return @@ State.set_buffer_data state buffer_filename data;
              ) in
          sexp ([%sexp_of: (Emacs_data.t list * Emacs_data.buffer_timestamp) option]
                  (Some (data.buffer_data, data.modification_time)))
        end
        else sexp ([%sexp_of: (Emacs_data.t list * Emacs_data.buffer_timestamp) option]
                     None)
      end
    );    
    Dream.post "/buffer/action" (fun req ->
      handle_error begin
        let%bind request_data = Lwt_result.ok @@ Dream.body req in
        let%bind buffer_filename, client_timestamp, pos, action =
          Result.try_with (fun () ->
            [%of_sexp: (string * Emacs_data.buffer_timestamp * int *
                        [`clock_in | `clock_out | `change_todo | `open_in_emacs ])]
              (Sexp.of_string request_data))
          |> Result.map_error ~f:(fun err -> `Msg ("Invalid data: " ^ Exn.to_string err))
          |> Lwt.return in
        let%bind last_modified_timestamp =
          Emacs.get_buffer_timestamp buffer_filename
          |> Lwt_result.map_error
               (fun _ -> `Msg "Emacs RPC failed - invalid filename?") in
        if Emacs_data.buffer_timestamp_gt client_timestamp last_modified_timestamp
        then sexp ([%sexp_of: unit option] None)
        else begin
          let command, focus = match action with
            | `clock_in -> "org-clock-in", false
            | `clock_out -> "org-clock-out", false
            | `change_todo -> "org-todo", false
            | `open_in_emacs -> "progn", true in
          let%bind () =
            Emacs.run_at_pos ~filename:buffer_filename ~pos ~focus command in
          sexp ([%sexp_of: unit option] (Some ()))
        end
      end
    )
  ]

let run ?emacsclient_cmd ?(debug=false) port =

  Option.iter ~f:Emacs.set_emacsclient_cmd emacsclient_cmd;

  let static_routes = Static.build_static_routes debug in

  Dream.run ?port begin
    (if debug then Dream.logger else Fun.id) @@
    Dream.router [
      api_routes;

      static_routes;

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
