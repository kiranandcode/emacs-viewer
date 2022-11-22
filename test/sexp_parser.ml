

open Core
open Cmdliner
open Emacs_viewer

let decode_file decoder inf =
  let s =
    In_channel.read_all inf
    |> Sexplib.Sexp.of_string in
  Decoders_sexplib.Decode.decode_value decoder s

let run ty infile =
  match ty, infile with
  | "property", inf ->
    let res = decode_file Org_data.property inf in
    print_endline @@ [%show: (Org_data.property, Org_data.D.error) result] res
  | "clock", inf ->
    let res = decode_file Org_data.clock inf in
    print_endline @@ [%show: (Org_data.t, Org_data.D.error) result] res
  | "planning", inf ->
    let res = decode_file Org_data.planning inf in
    print_endline @@ [%show: (Org_data.t, Org_data.D.error) result] res
  | "section", inf ->
    let res = decode_file Org_data.section inf in
    print_endline @@ [%show: (Org_data.t, Org_data.D.error) result] res
  | "org-buffer-data", inf ->
    let res = decode_file Org_data.org_buffer_data inf in
    print_endline @@ [%show: ((string * Org_data.t list) list, Org_data.D.error) result] res
  | _ ->
    Format.ksprintf failwith
      "unsupported sexp parser %s for file %s" ty infile

let ty =
  Arg.(required @@
       pos 0
         (some string) None
         (info ~docv:"TYPE" ~doc:"type of value to parse" []))

let infile =
  Arg.(required @@
       pos 1
         (some file) None
         (info ~docv:"FILE" ~doc:"file containing sexp to parse" [])
      )


let () =
  exit @@ Cmd.eval begin
    Cmd.v
      (Cmd.info "sexp_parser.exe")
      Term.(const run $ ty $ infile)
  end
