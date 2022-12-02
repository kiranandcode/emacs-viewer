let run client debug port =
  Emacs_viewer.Server.run ?emacsclient_cmd:client ~debug:debug port

open Cmdliner

let port =
  let info =
    Arg.info ~doc:"Port to run server on, defaults to 8080." ["p"; "port"] in
  Arg.value @@ Arg.opt Arg.(some int) None info

let client =
  let info =
    Arg.info ~doc:"Command to use for Emacsclient, defaults to emacsclient.emacs." ["c"; "client"]
  in
  Arg.value @@ Arg.opt Arg.(some string) None info

let debug =
  let info =
    Arg.info ~doc:"Whether to run in debug mode." ["D"; "debug"]
  in
  Arg.value @@ Arg.flag info


let () =
  exit @@ Cmd.eval @@ Cmd.v Cmd.(info "emacs-viewer") Term.(const run $ client $ debug $ port)
