module Sexp = Sexplib.Sexp

let emacs = Bos.Cmd.v "emacsclient.emacs"
let emacs_cmd s =
  let open Bos in
  OS.Cmd.run_out Cmd.(emacs % "-e" % s)
  |> OS.Cmd.to_string
  |> Result.map Sexplib.Sexp.of_string

let collect_org_buffers_cmd = {elisp|
(progn
  (let ((elements nil))
    (dolist (buffer (org-buffer-list))
      (with-current-buffer buffer
        (let ((visit-time (time-convert (visited-file-modtime) 'integer))
              (mod-time (buffer-modified-tick)))
          (push (list (buffer-file-name) visit-time mod-time) elements))
        )
      )
    elements))
|elisp}

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



let run port =
  Dream.run ?port begin
    Dream.router [
      Dream.get "/static/js/**" @@ Dream.static "_build/default/js";
      Dream.get "/static/styles/**" @@ Dream.static "_build/default/styles";
      Dream.get "/" (fun _req ->
        Dream.html {html|
          <!doctype html>
          <html lang="en">
             <head>
                 <meta charset="utf-8">
                 <meta name="viewport" content="width=device-width, initial-scale=1">
		 <title>Org Agenda • TodoMVC</title>
   		 <link rel="stylesheet" href="static/styles/style.css">
                 <script src="static/js/app.bc.js"></script>
             </head>
             <body>
                <div id="app">
                </div>
             </body>
          </html>
        |html}
      );
      Dream.get "/buffers" (fun _req ->
        match emacs_cmd collect_org_buffers_cmd with
        | Error (`Msg err) ->
          Dream.html ("error: " ^ err)
        | Ok sexp ->
          match Org_data.buffer_list sexp with
          | Error err ->
            Dream.html ("<html><head><title>Agenda - Error</title></head><body>"
                        ^ "<p> Error: " ^ Decoders.Error.to_string Sexp.pp_hum err
                        ^ "<br/><pre>"
                        ^ Sexp.to_string_hum sexp
                        ^ "</pre></body></html>")
          | Ok data ->
            let data = [%show:(string * Org_data.buffer_timestamp) list] data in
            Dream.html ("<html><head><title>Agenda</title></head><body>"
                        ^ "<br/><pre>"
                        ^ data
                        ^ "</pre></body></html>")
      );
      Dream.get "/todos" (fun _req ->
        print_endline "hello world";
        let cmd = emacs_cmd collect_todos_cmd in
        match cmd with
        | Error (`Msg err) ->
          Dream.html ("error: " ^ err)
        | Ok sexp ->
          match Org_data.org_buffer_data sexp with
          | Error err -> 
            Dream.html ("<html><head><title>Agenda - Error</title></head><body>"
                        ^ "<p> Error: " ^ Decoders.Error.to_string Sexp.pp_hum err
                        ^ "<br/><pre>"
                        ^ Sexp.to_string_hum sexp
                        ^ "</pre></body></html>")
          | Ok data ->
            let data = [%show:(string * Org_data.t list) list] data in
            Dream.html ("<html><head><title>Agenda</title></head><body>"
                        ^ "<br/><pre>"
                        ^ data
                        ^ "</pre></body></html>")
      );
      Dream.get "/" (fun _req ->
        print_endline "hello world";
        Dream.html "hello world"
      )
    ]
  end
