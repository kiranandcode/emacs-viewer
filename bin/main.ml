let run port =
  Emacs_viewer.Server.run port


open Cmdliner

let port =
  let info =
    Arg.info ~doc:"Port to run server on." ["p"; "port"] in
  Arg.value @@ Arg.opt Arg.(some int) None info

let () =
  exit @@ Cmd.eval @@ Cmd.v Cmd.(info "emacs-viewer") Term.(const run $ port)

