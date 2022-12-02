open Core
module Sexp = Sexplib.Sexp

let log = lazy (Dream.sub_log "command")

let lift_decoders_error res =
  Lwt_result.lift (
    Result.map_error ~f:(fun e -> `Msg (Decoders.Error.to_string Sexplib.Sexp.pp_hum e))
      res
  )

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
  let%bind data = lift_decoders_error (Decoder.buffer_list sxp) in
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
  let%bind data = lift_decoders_error (Decoder.buffer_timestamp sxp) in
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
    lift_decoders_error (Decoder.buffer_data sxp) in
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
