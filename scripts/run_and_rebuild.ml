[@@@warning "-32"]
open Core

let rebuild_timeout = 3.0

(** [is_source s] returns [true] if [s] is a valid source file. *)
let is_source s =
  (String.is_suffix ~suffix:".ml" s ||
   String.is_suffix ~suffix:".mli" s ||
   String.equal s "dune")
  && not (String.contains s '#')

let rebuild_timeout_passed current_time last_rebuild_time =
  let span = Ptime.diff current_time last_rebuild_time in
   Float.(rebuild_timeout <. Ptime.Span.to_float_s span)

let build_project () =
  let open Bos in
  let opam = Cmd.v "opam" in
  match OS.Cmd.run Cmd.(opam % "exec" % "--" % "dune" % "build" % "@server") with
  | Ok () -> true
  | Error (`Msg _) ->
    Format.printf "ERR: failed to build project\n%!";
    false

let run_project () =
  let proc =
    UnixLabels.create_process ~stdin:UnixLabels.stdin  ~stdout:UnixLabels.stdout ~stderr:UnixLabels.stderr
      ~prog:"opam" ~args:[| "opam"; "exec"; "--";  "dune"; "exec"; "./bin/main.exe"; "--"; "-D"|] in
  Pid.of_int proc

let process_still_running pid =
  match Core_unix.wait_nohang (`Pid pid) with
  | None -> true
  | Some (_, Ok ()) -> false
  | Some (_, Error (`Exit_non_zero n)) ->
    Format.printf "ERR: exited with status %d\n%!" n;
    false
  | Some (_, Error (`Signal s)) ->
    Format.printf "ERR: exited with signal %s\n%!" (Signal.to_string s);
    false

let kill_process pid =
  let open Bos in
  match Core_unix.wait_nohang (`Pid pid) with
  | None ->
    Format.printf "INFO: child process running, so will kill\n%!";
    let _ = OS.Cmd.run Cmd.(v "kill" % "-9" % (Pid.to_string pid)) in
    let _ = Core_unix.wait (`Pid pid) in
    Format.printf "INFO: killed child process\n%!";
    ()
  | Some (_, _) -> ()
  | exception UnixLabels.Unix_error _ -> ()


let inotify = Inotify.create ()
let watch = Inotify.add_watch inotify "./" [Inotify.S_Modify]
let watch = Inotify.add_watch inotify "./scripts" [Inotify.S_Modify]
let watch = Inotify.add_watch inotify "./styles" [Inotify.S_Modify; Inotify.S_Create]
let watch = Inotify.add_watch inotify "./data" [Inotify.S_Modify; Inotify.S_Create]
let watch = Inotify.add_watch inotify "./lib" [Inotify.S_Modify; Inotify.S_Create]
let watch = Inotify.add_watch inotify "./bin" [Inotify.S_Modify; Inotify.S_Create]
let watch = Inotify.add_watch inotify "./js" [Inotify.S_Modify; Inotify.S_Create]

let last_rebuild_time = ref None
let running_process_pid = ref None

let rec loop () =
  let ls = Inotify.read inotify in
  let source_change = ref false in
  List.iter ~f:(fun ((_, _, _, fname)) ->
    source_change := !source_change || Option.exists ~f:is_source fname
  ) ls;
  if !source_change || not @@ Option.exists ~f:process_still_running !running_process_pid then begin
    let (let+) x f = if x then f () in
    let current_time = Ptime_clock.now () in
    let+ () = Option.for_all !last_rebuild_time
                ~f:(rebuild_timeout_passed current_time) in
    (* pessimistically record the current time as the last time we rebuilt *)
    last_rebuild_time := Some current_time;
    let+ _ = build_project () in
    let current_time = Ptime_clock.now () in
    (* if rebuilding succeeded, update last rebuild time to be new current time  *)
    last_rebuild_time := Some current_time;
    (*  *)
    Format.printf "INFO: Source changed, restarting project\n%!";
    (* kill old process *)
    Option.iter ~f:kill_process !running_process_pid;
    running_process_pid := Some (run_project ());
  end;
  loop ()

let _ = Caml.at_exit (fun () -> Option.iter ~f:kill_process !running_process_pid)

let () = loop ()
