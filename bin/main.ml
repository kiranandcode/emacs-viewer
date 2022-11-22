module Sexp = Sexplib.Sexp

let emacs = Bos.Cmd.v "emacsclient.emacs"
let emacs_cmd s =
  let open Bos in
  OS.Cmd.run_out Cmd.(emacs % "-e" % s)
  |> OS.Cmd.to_string
  |> Result.map Sexplib.Sexp.of_string

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
      Dream.get "/todos" (fun _req ->
        print_endline "hello world";
        let cmd = emacs_cmd collect_todos_cmd in
        match cmd with
        | Error (`Msg err) ->
          Dream.html ("error: " ^ err)
        | Ok sexp ->
          Dream.html ("<html><head><title>Agenda</title></head><body><pre>" ^ Sexp.to_string_hum sexp ^ "</pre></body></html>")
      );
      Dream.get "/" (fun _req ->
        print_endline "hello world";
        Dream.html "hello world"
      )
    ]

  end


open Cmdliner

let port =
  let info =
    Arg.info ~doc:"Port to run server on." ["p"; "port"] in
  Arg.value @@ Arg.opt Arg.(some int) None info

let () =
  exit @@ Cmd.eval @@ Cmd.v Cmd.(info "emacs-viewer") Term.(const run $ port)

