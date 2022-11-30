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

let adiv ~cls contents =
  Vdom.Node.div ~attr:(Vdom.Attr.classes cls) contents

let mapM ls ~f =
  Value.map ls ~f:(fun ls -> 
    Int.Map.of_iteri_exn
      ~iteri:(fun ~f ->
        List.iteri ls ~f:(fun key data -> f ~key ~data)))
  |> Bonsai.assoc (module Int)
       ~f:(fun _ child -> f child)
  |> Computation.map ~f:(fun map ->
    Map.to_sequence ~order:`Increasing_key map
    |> Sequence.map ~f:(fun (_, vl) -> vl)
    |> Sequence.to_list)

let merge_list ~f ~merge ls =
  let rec loop ~f (merged, acc) = function
    | h :: t ->
      begin match f h with
      | Either.First vl -> loop ~f (vl :: merged, acc) t
      | Second vl -> match merged with
        | [] -> loop ~f ([], vl :: acc) t
        | merged -> loop ~f ([], vl :: merge (List.rev merged) :: acc) t
      end
    | [] ->
      match merged with
      | [] -> List.rev acc
      | _ -> List.rev (merge (List.rev merged) :: acc) in
  loop ~f ([], []) ls

let render_time (ts: Emacs_data.Data.time) =
  let time = Format.sprintf "%02d-%02d-%02d%s" ts.year ts.month ts.day
    (match ts.hour, ts.minute with
     | Some hour, Some minute -> Format.sprintf " %02d:%02d" hour minute
     | Some hour, None -> Format.sprintf " %02d:00" hour
     | None, Some minute -> Format.sprintf " 00:%02d" minute
     | None, None -> "") in
  Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-time") Vdom.Node.[p [text time]]

let render_timestamp (ts: Emacs_data.Data.timestamp) =
  Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-timestamp") [
    render_time ts.start;
    render_time ts.end_;
  ]

let rec render_text (txt: Emacs_data.Data.txt) =
  let to_class = function
    | Emacs_data.Data.Italic -> "italic"
    | Emacs_data.Data.Strikethrough -> "strikethrough" | Emacs_data.Data.Bold -> "bold"
    | Emacs_data.Data.Underline -> "underline" | Emacs_data.Data.Subscript -> "subscript"
    | Emacs_data.Data.Superscript -> "superscript" in
  let open Vdom.Node in
  match txt with
  | Emacs_data.Data.Lit txt -> p [text txt]
  | Emacs_data.Data.Timestamp ts -> render_timestamp ts
  | Emacs_data.Data.Concat elts ->
    let elts = List.map ~f:render_text elts in
    div elts
  | Emacs_data.Data.StatisticsCookie txt ->
    p ~attr:(Vdom.Attr.class_ "org-text-stats-cookie") [text txt]
  | Emacs_data.Data.Code cde ->
    pre [text cde]
  | Emacs_data.Data.Verbatim verb ->
    pre [text verb]
  | Emacs_data.Data.Entity ent ->
    p ~attr:(Vdom.Attr.class_ "org-text-entity") [text ent]
  | Emacs_data.Data.Format (fmt, t) ->
    div ~attr:(Vdom.Attr.class_ (to_class fmt)) [render_text t]
  | Emacs_data.Data.InlineSrcBlock { language; value } ->
    pre [code ~attr:(Vdom.Attr.class_ ("language-" ^ language)) [text value]]

let render_todo ({ keyword; ty }: Emacs_data.Data.todo) =
  let todo_class = match ty with `Done -> "org-mode-todo-done" | `Todo -> "org-mode-todo-todo" in
  adiv ~cls:["org-mode-todo"; todo_class] [ Vdom.Node.text keyword ]

let rec data_to_view : Emacs_data.Data.t Value.t -> Vdom.Node.t Computation.t = fun data ->
  match%sub data with
  | Emacs_data.Data.Property { key; value; pos=_ } ->
    let%arr key = key
    and value = value in
    Vdom.Node.(
      adiv ~cls:["org-data"; "org-property"] [
        adiv ~cls:["org-property-key"] [ text key ];
        adiv ~cls:["org-property-value"] [ text value ];
      ]
    )
  | Emacs_data.Data.Section { pos=_; properties } ->
    let%sub children = Bonsai.lazy_ @@ lazy (mapM ~f:data_to_view properties) in
    let%arr children = children in
    adiv ~cls:["org-mode-section"; "org-mode-children";] children
  | Emacs_data.Data.Headline {
    title;
    raw_value=_;
    pos=_; level;
    priority=_; 
    tags;
    todo;
    subsections;
    closed=(_: Emacs_data.Data.timestamp option);
    scheduled=(_: Emacs_data.Data.timestamp option);
    deadline=(_: Emacs_data.Data.timestamp option);
  } ->
    let%sub subsections = Bonsai.lazy_ @@ lazy (mapM ~f:data_to_view subsections) in
    let%arr subsections = subsections
    and title = title
    and level = level
    and tags = tags
    and todo = todo in
    adiv ~cls:["org-mode-headline"; ("org-mode-headline-" ^ Int.to_string level)] [
      adiv ~cls:["org-mode-headline-contents"] [
        adiv ~cls:["org-mode-title"]
          (match todo with
             None -> [adiv ~cls:["org-mode-title-text"] [(render_text title)]]
           | Some todo ->
             [adiv ~cls:["org-mode-title-todo"] [render_todo todo];
              adiv ~cls:["org-mode-title-text"] [render_text title]]                      
          );
        adiv ~cls:["org-mode-tags"] (List.map ~f:(fun v -> Vdom.Node.(p [text v])) tags);
      ];
      adiv ~cls:["org-mode-headline-childen";"org-mode-children"] subsections
    ]
  | Emacs_data.Data.Drawer { name; pos=_; contents } ->
    let%sub contents = Bonsai.lazy_ @@ lazy (mapM ~f:data_to_view contents) in
    let%arr contents = contents
    and name = name in
    adiv ~cls:["org-mode-drawer"] [
      adiv ~cls:["org-mode-drawer-name"] [Vdom.Node.text name];
      adiv ~cls:["org-mode-drawer-children";"org-mode-children"] contents;
    ]
  | Emacs_data.Data.Clock {status; value;duration; pos=_} ->
    let%arr status = status
    and value = value
    and duration = duration in
    let clock_class = match status with
      | `closed -> "org-mode-clock-closed"
      | `running -> "org-mode-clock-running" in
    adiv ~cls:["org-mode-clock";clock_class] [
      adiv ~cls:["org-mode-clock-status"] [
        Vdom.Node.text (match status with
          | `closed -> "closed"
          | `running -> "running"
        )];
      adiv ~cls:["org-mode-clock-duration"] (match duration with None -> []
                                                               | Some duration ->
                                                                 [Vdom.Node.text duration]);
      adiv ~cls:["org-mode-clock-value"] [(render_timestamp value)];
    ]
  | Emacs_data.Data.Planning {
    closed; scheduled; deadline; pos=_
  } ->
    let%arr closed = closed
    and scheduled = scheduled
    and deadline = deadline in
    adiv ~cls:["org-mode-planning"] (List.concat [
      (match closed with None -> [] | Some closed ->
         [adiv ~cls:["org-mode-planning-timestamp"; "org-mode-planning-closed"]
            [render_timestamp closed]]);
      (match scheduled with None -> [] | Some scheduled ->
         [adiv ~cls:["org-mode-planning-timestamp"; "org-mode-planning-scheduled"]
            [render_timestamp scheduled]]);
      (match deadline with None -> [] | Some deadline ->
         [adiv ~cls:["org-mode-planning-timestamp"; "org-mode-planning-deadline"]
            [render_timestamp deadline]]);
    ])


let current_buffer_to_view current_buffer =
  match%sub current_buffer with
  | None ->
    return @@ (Value.return @@ adiv ~cls:[] [Vdom.Node.text "Empty"])
  | Some buffer ->
    let%sub buffer_entries = mapM ~f:data_to_view (Value.map ~f:Buffer.elements buffer) in
    let%arr buffer_entries = buffer_entries in
    adiv ~cls:["org-mode-current-buffer"] buffer_entries

let bufferlist_to_view ~set_current_buffer ~set_state ~reload_state
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
      div ~attr:(Vdom.Attr.classes [ "navigation-bar"]) (List.concat [
        [div ~attr:(Vdom.Attr.classes ["buffer-list"]) @@
         List.map ~f:(fun (fname, simple_name) ->
           let name = match simple_name with
               None -> String.split_on_chars ~on:['/'] fname |> List.last_exn
             | Some name -> name in
           div ~attr:Vdom.Attr.(many_without_merge [
             classes ["navigation-button"];
             on_click (fun _ -> set_current_buffer set_state model fname)
           ]) [
             a ~attr:(Vdom.Attr.classes ["buffer"; "buffers"]) [text name]
           ]
         ) buffers];
        [div ~attr:(Vdom.Attr.classes ["navigation-buttons"])
           [div ~attr:Vdom.Attr.(many_without_merge [
           classes ["navigation-button"];
           on_click (fun _ -> reload_state set_state)
         ]) [
           a ~attr:(Vdom.Attr.classes ["buffer-refresh"]) [text "↻"];
         ]]];
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
  let reload_state set_state =
    let%bind.Effect data = get_buffer_list () in
    set_state (BufferList.init_opt data) in
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
      bufferlist_to_view ~set_current_buffer ~reload_state ~set_state model in
    return buffer_list

let application =
  let%sub view = view in
  let%arr buffers = view in
  Vdom.Node.div [buffers]

let (_: _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app"
    application


