[@@@warning "-32"]
open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module Model = struct

  type t = {
    buffers: (string * Emacs_data.Data.buffer_timestamp) list;
  } [@@deriving sexp, fields, equal]

  let init_opt data =
    match data with
    | Ok data -> Some {buffers=data}
    | Error _ -> None

  let no_buffers_view =
    let open Vdom.Node in
    Value.return @@ div ~attr:Vdom.Attr.(many_without_merge [
      classes ["hero"; "main-info"]
    ]) [
      div ~attr:(Vdom.Attr.class_ "info-bold") [
        text "No org buffers currently open.";
      ];
      div ~attr:(Vdom.Attr.class_ "info-it") [
        text "Open one to get started...";
      ]
    ]

  let to_view (model: t Value.t) =
    let%arr buffers = Value.map ~f:buffers model in
    let open Vdom.Node in
    div ~attr:(Vdom.Attr.classes ["buffer-list"; "navigation-bar"]) (List.concat [
      List.map ~f:(fun (name, _) ->
        a ~attr:(Vdom.Attr.classes ["buffer"; "buffers"]) [text name]
      ) buffers
    ])
    

end

let get_buffer_list =
  Bonsai_web.Effect.of_deferred_fun (fun () -> 
    Async_kernel.Deferred.map
      ~f:(fun s ->
        Or_error.map ~f:(fun s ->
          [%of_sexp: (string * Emacs_data.Data.buffer_timestamp) list] @@
          Sexp.of_string s
        ) s)
      (Async_js.Http.get "/api/buffers"))

let get_buffer name =
  Bonsai_web.Effect.of_deferred_fun (fun () ->
    Async_kernel.Deferred.map
      ~f:(fun s ->
        Or_error.map ~f:(fun s ->
          [%of_sexp: (string * Emacs_data.Data.buffer_timestamp) list] @@
          Sexp.of_string s
        ) s)
      (Async_js.Http.post ~body:(String name) "/api/buffer")
  )

let view =
  let%sub state, set_state = Bonsai.state_opt [%here] (module Model) in
  let%sub on_activate =
    let%arr set_state = set_state in
    let%bind.Effect data = get_buffer_list () in
    set_state (Model.init_opt data) in
  let%sub () = Bonsai.Edge.lifecycle ~on_activate () in
  match%sub state with
  | None ->
    return Model.no_buffers_view
  | Some model ->
    Model.to_view model

let application =
  let%sub view = view in
  let%arr buffers = view in
  Vdom.Node.div [buffers]

let (_: _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app"
    application

