[@@@warning "-32"]
open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module Buffer = struct
  type t = {
    elements: Emacs_data.Data.t list
  } [@@deriving sexp, fields, equal]

  let init_opt data = match data with
    | Ok data -> Some {elements=data}
    | Error _ -> None
end

module BufferList = struct

  type t = {
    buffers: (string * string option) list;
    current_buffer: Buffer.t option;
  } [@@deriving sexp, fields, equal]

  let init_opt data =
    match data with
    | Ok data -> Some {buffers=data; current_buffer=None}
    | Error _ -> None

  let set_current_buffer t buffer =
    {t with current_buffer=Some buffer}

end

let loading_view =
  let open Vdom.Node in
  Value.return @@ div ~attr:Vdom.Attr.(many_without_merge [
    classes ["hero"; "main-info"]
  ]) [
    div ~attr:(Vdom.Attr.class_ "info-loading") [
      text "Loading...";
    ];
  ]

let current_buffer_to_view current_buffer =
  let%arr buffer = current_buffer in
  let open Vdom.Node in
  match (buffer: _ Option.t) with
  | None ->
    div [text "Empty"]
  | Some buffer ->
    div 
      (List.map buffer.Buffer.elements ~f:(function
           Emacs_data.Data.Property { key; value; pos=_ } ->
           div [ div [text "PROPERTY"]; div [text key]; div [text value]]
         | Emacs_data.Data.Section { pos=_; properties=_ } ->
           div [ div [text "SECTION"] ]
         | Emacs_data.Data.Headline hdline ->
           div [ div [text "HEADLINE"]; div [text hdline.raw_value] ]
         | Emacs_data.Data.Drawer { name; pos=_; contents=_ } ->
           div [ div [text "DRAWER"]; div [text name] ]
         | Emacs_data.Data.Clock _ ->
           div [ div [text "CLOCK"] ]
         | Emacs_data.Data.Planning _ ->
           div [ div [text "PLANNING"] ]
       ))


let bufferlist_to_view ~set_current_buffer ~set_state
      (model: BufferList.t Value.t) : Vdom.Node.t Computation.t =
  let%sub current_buffer = return (Value.map ~f:BufferList.current_buffer model) in
  let%sub current_buffer_view = current_buffer_to_view current_buffer in
  let%arr buffers = Value.map ~f:BufferList.buffers model
  and current_buffer = current_buffer_view
  and model = model 
  and set_state = set_state in
  let open Vdom.Node in
  match buffers with
  | [] ->
    div ~attr:Vdom.Attr.(many_without_merge [
      classes ["hero"; "main-info"]
    ]) [
      div ~attr:(Vdom.Attr.class_ "info-bold") [
        text "No org buffers currently open.";
      ];
      div ~attr:(Vdom.Attr.class_ "info-it") [
        text "Open one to get started...";
      ]
    ]
  | buffers ->
    div ~attr:(Vdom.Attr.class_ "main") [
      div ~attr:(Vdom.Attr.classes ["buffer-list"; "navigation-bar"]) (List.concat [
        List.map ~f:(fun (fname, simple_name) ->
          let name = match simple_name with None -> fname | Some name -> name in
          div ~attr:Vdom.Attr.(many_without_merge [
            classes ["navigation-button"];
            on_click (fun _ -> set_current_buffer set_state model fname)
          ]) [
            a ~attr:(Vdom.Attr.classes ["buffer"; "buffers"]) [text name]
          ]
        ) buffers;
        [div ~attr:(Vdom.Attr.classes ["navigation-button"]) [
           a ~attr:(Vdom.Attr.classes ["buffer-refresh"]) [text "↻"];
         ]];
      ]);
      current_buffer
    ]

let get_buffer_list =
  Bonsai_web.Effect.of_deferred_fun (fun () -> 
    Async_kernel.Deferred.map
      ~f:(fun s ->
        Or_error.map ~f:(fun s ->
          [%of_sexp: (string * string option) list] @@
          Sexp.of_string s
        ) s)
      (Async_js.Http.get "/api/buffers"))

let get_buffer =
  Bonsai_web.Effect.of_deferred_fun (fun name ->
    Async_kernel.Deferred.map
      ~f:(fun s ->
        Or_error.map ~f:(fun s ->
          [%of_sexp: Emacs_data.Data.t list] @@
          Sexp.of_string s
        ) s)
      (Async_js.Http.post ~body:(String name) "/api/buffer")
  )

let view =
  let%sub state, set_state = Bonsai.state_opt [%here] (module BufferList) in
  let set_current_buffer set_state state name =
    let%bind.Effect buffer = get_buffer name in
    match Buffer.init_opt buffer with
    | Some buffer ->
      let state = BufferList.set_current_buffer state buffer in
      set_state (Some state)
    | None -> Ui_effect.return () in
  let%sub on_activate =
    let%arr set_state = set_state in
    let%bind.Effect data = get_buffer_list () in
    set_state (BufferList.init_opt data) in
  let%sub () = Bonsai.Edge.lifecycle ~on_activate () in
  match%sub state with
  | None ->
    return loading_view
  | Some model ->
    let%sub buffer_list =
      bufferlist_to_view ~set_current_buffer:(set_current_buffer) ~set_state model in
    return buffer_list

let application =
  let%sub view = view in
  let%arr buffers = view in
  Vdom.Node.div [buffers]

let (_: _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app"
    application


