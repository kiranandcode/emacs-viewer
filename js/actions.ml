[@@@warning "-32"]
open! Core

let get_buffer_list_def () =
  Async_kernel.Deferred.map
    ~f:(fun s ->
      Or_error.map ~f:(fun s ->
        [%of_sexp: (string * string option) list] @@
        Sexp.of_string s
      ) s)
    (Async_js.Http.get "/api/buffers")  

let get_buffer_def name =
  Async_kernel.Deferred.map
    ~f:(fun s ->
      Or_error.map ~f:(fun s ->
        let (data, timestamp) =
          [%of_sexp: Emacs_data.t list * Emacs_data.buffer_timestamp] @@
          Sexp.of_string s in
        (name, data, timestamp)
      ) s)
    (Async_js.Http.post ~body:(String name) "/api/buffer/load")

let reload_buffer_def data =
  let data = [%sexp_of: (string *
                         Emacs_data.buffer_timestamp)] data
             |> Sexp.to_string_mach in
  Async_kernel.Deferred.map
    ~f:(fun s ->
      Or_error.map ~f:(fun s ->
        [%of_sexp:
          (Emacs_data.t list *
           Emacs_data.buffer_timestamp) option] @@
        Sexp.of_string s
      ) s)
    (Async_js.Http.post ~body:(String data) "/api/buffer/reload")

let reload_buffer_list_def current_buffer_name =
  let open Async_kernel.Deferred.Result.Let_syntax in
  let%bind buffer_list = get_buffer_list_def () in
  let new_current_buffer =
    match current_buffer_name with
    | Some current_buffer_name when
        List.exists ~f:(fun (filename, _) ->
          String.equal current_buffer_name filename)
          buffer_list ->
      Some current_buffer_name
    | _ -> Option.map ~f:fst (List.hd buffer_list) in
  match new_current_buffer with
  | None -> Async_kernel.Deferred.Result.return (buffer_list, None)
  | Some current_buffer_name ->
    let%bind data = get_buffer_def current_buffer_name in
    Async_kernel.Deferred.Result.return
      (buffer_list, Some data)

let run_action_def (data: string * Emacs_data.buffer_timestamp * int *
                          [ `change_todo | `clock_in | `clock_out | `open_in_emacs ]) =
  let data = [%sexp_of: (string * Emacs_data.buffer_timestamp * int *
                         [`clock_in | `clock_out | `change_todo | `open_in_emacs ])] data
             |> Sexplib.Sexp.to_string_mach in
  Async_kernel.Deferred.map
    ~f:(fun s ->
      Or_error.map ~f:(fun s ->
        [%of_sexp: unit option] @@
        Sexp.of_string s
      ) s)
    (Async_js.Http.post ~body:(String data) "/api/buffer/action")

let get_buffer_list =
  Bonsai_web.Effect.of_deferred_fun get_buffer_list_def

let reload_buffer_list =
  Bonsai_web.Effect.of_deferred_fun reload_buffer_list_def

let get_buffer =
  Bonsai_web.Effect.of_deferred_fun get_buffer_def

let reload_buffer =
  Bonsai_web.Effect.of_deferred_fun reload_buffer_def

let run_action :
  string * Emacs_data.buffer_timestamp * int *
  [< `change_todo | `clock_in | `clock_out | `open_in_emacs ] ->
  unit option Or_error.t Ui_effect.t =
  Bonsai_web.Effect.of_deferred_fun run_action_def
