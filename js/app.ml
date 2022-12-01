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

let rec subsection_contains_active_clock : Emacs_data.Data.t -> bool =
  function
  | Emacs_data.Data.Property _ -> false
  | Emacs_data.Data.Section { pos=_; properties } ->
    List.exists ~f:subsection_contains_active_clock properties
  | Emacs_data.Data.Headline _ -> false
  | Emacs_data.Data.Drawer { name="LOGBOOK"; pos=_; contents } ->
    List.exists ~f:subsection_contains_active_clock contents
  | Emacs_data.Data.Drawer { name=_; pos=_; contents=_ } -> false
  | Emacs_data.Data.Clock {status;_} ->
    begin match status with `closed -> false | `running -> true end
  | Emacs_data.Data.Planning _ -> false

let get_active_clock : Emacs_data.Data.t -> _ option =
  let exception FoundClock of
      (Emacs_data.Data.timestamp * string option * Emacs_data.Data.pos) in
  let rec loop =
    function
    | Emacs_data.Data.Property _ -> ()
    | Emacs_data.Data.Section { pos=_; properties } ->
      List.iter ~f:loop properties
    | Emacs_data.Data.Headline _ -> ()
    | Emacs_data.Data.Drawer { name="LOGBOOK"; pos=_; contents } ->
      List.iter ~f:loop contents
    | Emacs_data.Data.Drawer { name=_; pos=_; contents=_ } -> ()
    | Emacs_data.Data.Clock {status; value; duration; pos} ->
      begin match status with `closed -> () | `running ->
        raise (FoundClock (value,duration,pos)) end
    | Emacs_data.Data.Planning _ -> () in
  fun data -> try loop data; None with FoundClock (value, duration, pos) ->
    Some (value,duration,pos)

let rec subsection_contains_matching ~hide_completed ~only_clocked ~search_text ~filter_tags
  : Emacs_data.Data.t -> bool =
  function
  | Emacs_data.Data.Property _ -> false
  | Emacs_data.Data.Section { pos=_; properties } ->
    List.exists ~f:(subsection_contains_matching
                      ~hide_completed ~only_clocked ~search_text ~filter_tags)
      properties
  | Emacs_data.Data.Headline data ->
    let is_completed = match data.todo with
      | None -> false
      | Some td -> match td.ty with `Todo -> false | `Done -> true  in
    let has_clock = List.exists ~f:subsection_contains_active_clock data.subsections in
    let clock_matches = not only_clocked || has_clock in
    let completed_matches = not (hide_completed && is_completed) in
    let text_matches =
      if String.is_empty search_text
      then true
      else Fuzzy_match.is_match
             ~char_equal:Char.Caseless.equal
             ~pattern:search_text data.raw_value in 
    let tags_match =
      if List.is_empty filter_tags
      then true
      else List.for_all ~f:(fun tag -> List.mem ~equal:String.equal data.tags tag) filter_tags in
    (completed_matches && clock_matches && text_matches && tags_match)
    || List.exists ~f:(subsection_contains_matching
                         ~hide_completed ~only_clocked ~search_text ~filter_tags)
         data.subsections
  | Emacs_data.Data.Drawer _ 
  | Emacs_data.Data.Clock _
  | Emacs_data.Data.Planning _ -> false

let rec data_to_view ~hide_completed ~only_clocked  ~search_text ~filter_tags ~add_tag :
  Emacs_data.Data.t Value.t -> Vdom.Node.t Computation.t = fun data ->
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
    let%sub children = Bonsai.lazy_ @@
      lazy (mapM ~f:(data_to_view  ~only_clocked ~hide_completed ~search_text ~filter_tags ~add_tag) properties) in
    let%arr children = children in
    adiv ~cls:["org-mode-section"; "org-mode-children";] children
  | Emacs_data.Data.Headline {
    title;
    raw_value;
    pos=_; level;
    priority=_; 
    tags;
    todo;
    subsections;
    closed=(_: Emacs_data.Data.timestamp option);
    scheduled=(_: Emacs_data.Data.timestamp option);
    deadline=(_: Emacs_data.Data.timestamp option);
  } ->
    let%sub should_fold, set_should_fold =
      Bonsai.state_machine0 [%here] (module Bool) (module Unit) ~default_model:false
        ~apply_action:(fun ~inject:_ ~schedule_event:_ model () -> not model) in
    let%sub text_matches =
      let%arr search_text = search_text
      and raw_value = raw_value in
      if String.is_empty search_text
      then true
      else Fuzzy_match.is_match
             ~char_equal:Char.Caseless.equal
             ~pattern:search_text raw_value in 
    let%sub tags_match =
      let%arr filter_tags = filter_tags
      and tags = tags in
      if List.is_empty filter_tags
      then true
      else List.for_all ~f:(fun tag -> List.mem ~equal:String.equal tags tag) filter_tags in
    let%sub completed_matches =
      let%arr hide_completed = hide_completed
      and todo = todo in
      let is_completed = match todo with
        | None -> false
        | Some td -> match td.ty with `Todo -> false | `Done -> true in
      not (hide_completed && is_completed) in
    let%sub has_clock =
      let%arr subsections = subsections in
      List.exists ~f:subsection_contains_active_clock subsections in
    let%sub clock_matches =
      let%arr only_clocked = only_clocked
      and has_clock = has_clock in
      not only_clocked || has_clock in
    let subsections_match =
      let%arr subsections = subsections
      and search_text = search_text
      and filter_tags = filter_tags
      and only_clocked = only_clocked
      and hide_completed = hide_completed in
      List.exists ~f:(subsection_contains_matching ~only_clocked ~hide_completed
                        ~search_text ~filter_tags) subsections in
    let%sub immediate_match =
      let%arr text_matches = text_matches and tags_match = tags_match
      and clock_matches = clock_matches and completed_matches = completed_matches in
      clock_matches && completed_matches && text_matches && tags_match in
    let%sub should_show =
      if%sub immediate_match
      then return (Value.return true)
      else subsections_match in
    if%sub should_show
    then begin
      let%sub clock_run_time =
        if%sub has_clock
        then
          let%sub current_time =
            Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 2.0) in
          let%arr subsections = subsections
          and current_time = current_time in
          match List.find_map ~f:get_active_clock subsections with
          | None -> None
          | Some (time,_duration,_pos) ->
            Option.map ~f:(fun clock_start ->
              Time_ns.diff current_time clock_start
              |> Time_ns.Span.to_short_string
            ) @@
            Option.first_some
              (Emacs_data.Data.time_to_time_ns time.start)
              (Emacs_data.Data.time_to_time_ns time.end_) 
        else return (Value.return None) in
      let subsections = Bonsai.lazy_ @@
        lazy (mapM ~f:(data_to_view ~only_clocked ~hide_completed ~search_text ~filter_tags ~add_tag) subsections) in
      let%sub fold_button =
        if%sub should_fold
        then return (Value.return "˅")
        else return (Value.return "᠁") in
      let%sub fold_button_class =
        if%sub should_fold
        then return (Value.return "fold-action-folded")
        else return (Value.return "fold-action-unfolded") in
      let%sub subsections =
        if%sub should_fold then return (Value.return []) else subsections in

      let%sub fold_buttons_panel =
        let%arr subsections = subsections
        and toggle_fold = set_should_fold
        and fold_button = fold_button
        and fold_button_class = fold_button_class in
        if List.is_empty subsections
        then []
        else [
          adiv ~cls:["org-mode-section-fold-buttons"] [
            Vdom.Node.div ~attr:(Vdom.Attr.many_without_merge [
              Vdom.Attr.on_click (fun _ -> toggle_fold ());
              Vdom.Attr.classes ["org-mode-section-button"; fold_button_class]
            ]) [
              Vdom.Node.(a [text fold_button]);
            ];
          ]] in
      let%arr subsections = subsections
      and title = title
      and level = level
      and tags = tags
      and todo = todo
      and add_tag = add_tag
      and clock_run_time = clock_run_time
      and fold_buttons_panel = fold_buttons_panel in
      Vdom.Node.div
        ~attr:Vdom.Attr.(many_without_merge [
          classes ["org-mode-headline"; ("org-mode-headline-" ^ Int.to_string level)];
        ]) [
        Vdom.Node.div
          ~attr:Vdom.Attr.(many_without_merge [
            classes ["org-mode-headline-contents"];
          ]) [
          Vdom.Node.div
            ~attr:Vdom.Attr.(many_without_merge [
              classes ["org-mode-title"];
              (* on_click (fun _ev -> toggle_fold ()) *)
            ])
            (List.concat [
               (match todo with
                | None -> []
                | Some todo ->
                 [adiv ~cls:["org-mode-title-todo"] [render_todo todo]]);
               [adiv ~cls:["org-mode-title-text"] [render_text title]];
               (match clock_run_time with
                | None -> []
                | Some run_time ->
                  [adiv ~cls:["org-mode-title-clock"] [Vdom.Node.text run_time]]);
               [adiv ~cls:["org-mode-section-buttons"] ([
                  adiv ~cls:["org-mode-section-action-buttons"] [
                    adiv ~cls:["org-mode-section-button"] [ Vdom.Node.(a [text "📂"]); ];
                    adiv ~cls:["org-mode-section-button"] [ Vdom.Node.(a [text "🕓"]); ];
                    adiv ~cls:["org-mode-section-button"] [ Vdom.Node.(a [text "🞋"]); ];
                  ];
                ] @ fold_buttons_panel)]
             ]);
          adiv ~cls:["org-mode-section-details"] [
            adiv ~cls:["org-mode-tags"] (List.map ~f:(fun v ->
              Vdom.Node.(p ~attr:(Vdom.Attr.on_click (fun _ -> add_tag v)) [text v])) tags
            );
          ]
        ];
        adiv ~cls:["org-mode-headline-childen";"org-mode-children"] subsections
      ]
    end
    else return (Value.return (Vdom.Node.text ""))
  | Emacs_data.Data.Drawer { name; pos=_; contents } ->
    let%sub contents = Bonsai.lazy_ @@
      lazy (mapM ~f:(data_to_view ~only_clocked ~hide_completed ~search_text ~filter_tags ~add_tag) contents) in
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


let current_buffer_to_view
      ~hide_completed ~only_clocked ~search_text ~filter_tags ~add_tag current_buffer =
  match%sub current_buffer with
  | None ->
    return @@ (Value.return @@
               adiv ~cls:["org-mode-current-buffer";
                          "org-mode-empty-buffer";
                          "centered-column"] [Vdom.Node.text "Please select a buffer..."])
  | Some buffer ->
    let%sub buffer_entries =
      mapM ~f:(data_to_view ~only_clocked ~hide_completed ~search_text ~filter_tags ~add_tag)
        (Value.map ~f:Buffer.elements buffer) in
    let%arr buffer_entries = buffer_entries in
    adiv ~cls:["org-mode-current-buffer"; "centered-column"] buffer_entries

let navigation_bar_view set_current_buffer reload_state buffers =
  let open Vdom.Node in
  div ~attr:(Vdom.Attr.classes [ "navigation-bar"; "centered-column"]) (List.concat [
    [div ~attr:(Vdom.Attr.classes ["buffer-list"]) @@
     List.map ~f:(fun (fname, simple_name) ->
       let name = match (simple_name: _ Option.t) with
           None -> String.split_on_chars ~on:['/'] fname |> List.last_exn
         | Some name -> name in
       div ~attr:Vdom.Attr.(many_without_merge [
         classes ["navigation-button"];
         on_click (fun _ -> set_current_buffer fname)
       ]) [
         a ~attr:(Vdom.Attr.classes ["buffer"; "buffers"]) [text name]
       ]
     ) buffers];
    [div ~attr:(Vdom.Attr.classes ["navigation-buttons"])
       [div ~attr:Vdom.Attr.(many_without_merge [
          classes ["navigation-button"];
          on_click (fun _ -> reload_state ())
        ]) [
          a ~attr:(Vdom.Attr.classes ["buffer-refresh"]) [text "↻"];
        ]]];
  ])

module S = [%css.raw {|
  .configuration_buttons {
     display: flex;
  }
|}]

let tag_split_re = Re.compile (Re.Pcre.re ":[^ ]*?(:[^ ]*?)*:")
let split_tags s =
  let text, tags =
    Re.split_full tag_split_re s
    |> List.partition_map ~f:(function
        `Delim ts -> Either.Second (String.concat_array @@ Re.Group.all ts)
      | `Text s -> Either.First s) in
  String.concat text, (String.split_on_chars ~on:[':'] (String.concat tags)
                       |> List.filter ~f:(fun f -> not @@ String.is_empty f))

let tagged_search_bar =
  let%sub tags_list, update_tags =
    Bonsai.state_machine0 [%here]
      (module String.Set)
      (module struct
        type t = [`AddTag of string | `AddTags of string list | `RemoveTag of string | `Reset]
        [@@deriving sexp_of]
      end)
      ~default_model:String.Set.empty
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model -> function
        | `AddTag s -> String.Set.add model s
        | `AddTags s -> List.fold_left ~init:model ~f:(String.Set.add) s
        | `RemoveTag s -> String.Set.remove model s
        | `Reset -> String.Set.empty
      ) in
  let%sub text, update_text = Bonsai.state_machine1 [%here]
                                (module String)
                                (module struct
                                  type t = [`SetText of string | `Reset]
                                  [@@deriving sexp_of]
                                end)
                                ~default_model:""
                                ~apply_action:(fun ~inject:_ ~schedule_event update_tags _ -> function
                                  | `SetText text ->
                                    let text, tags = split_tags text in
                                    schedule_event (update_tags (`AddTags tags));
                                    text
                                  | `Reset -> schedule_event (update_tags (`Reset)); ""
                                ) update_tags in
  let render_tag ~clear_tag name =
    let open Vdom.Node in
    div ~attr:(Vdom.Attr.class_ "configuration-search-bar-tag") [
      div ~attr:(Vdom.Attr.class_ "configuration-search-bar-tag-name") [
        text name
      ];
      div ~attr:(Vdom.Attr.many_without_merge [
        Vdom.Attr.class_ "configuration-search-bar-tag-button";
        Vdom.Attr.on_click (fun _ -> clear_tag name)
      ]) [ text "⛌" ];
    ] in
  let%arr search_text = text
  and tags_list = Value.map ~f:String.Set.to_list tags_list
  and update_text = update_text
  and update_tags = update_tags in
  let reset_input () = update_text `Reset in
  let add_tag t = update_tags (`AddTag t) in
  let open Vdom.Node in
  (search_text, tags_list, reset_input, add_tag),
  div ~attr:(Vdom.Attr.class_ "configuration-search-bar") [
    span ~attr:(Vdom.Attr.class_ "configuration-search-bar-tags") (
      List.map ~f:(render_tag ~clear_tag:(fun name -> update_tags (`RemoveTag name)))
        tags_list
    );
    input ~attr:(Vdom.Attr.many_without_merge [
      Vdom.Attr.value_prop search_text;
      Vdom.Attr.on_input (fun _ s -> update_text (`SetText s));
      Vdom.Attr.on_change (fun _ s -> update_text (`SetText s));
    ]) []
  ]


let bufferlist_to_view ~set_current_buffer ~set_state ~reload_state
      (model: BufferList.t Value.t) : Vdom.Node.t Computation.t =
  let%sub current_buffer = return (Value.map ~f:BufferList.current_buffer model) in
  let%sub (search_text, tags, clear_input, add_tag), search_bar = tagged_search_bar in
  let%sub hide_completed, toggle_hide_completed =
    Bonsai.state_machine0 [%here] (module Bool) (module Unit) ~default_model:false
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model () -> not model) in
  let%sub only_clocked, toggle_only_clocked =
    Bonsai.state_machine0 [%here] (module Bool) (module Unit) ~default_model:false
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model () -> not model) in

  let%sub current_buffer_view =
    current_buffer_to_view ~hide_completed ~only_clocked
      ~search_text ~filter_tags:tags ~add_tag current_buffer in

  let%sub hide_completed_button =
    let%sub icon = if%sub hide_completed
      then return (Value.return "configuration-button-active")
      else return (Value.return "configuration-button-inactive") in
    let%arr icon = icon
    and toggle_hide_completed = toggle_hide_completed in
    Vdom.Node.div ~attr:(Vdom.Attr.classes ["configuration-button"; icon]) [
      Vdom.Node.a ~attr:(Vdom.Attr.on_click (fun _ -> toggle_hide_completed ()))
        [Vdom.Node.text "!"];
    ] in

  let%sub only_clocked_button =
    let%sub cls = if%sub only_clocked
      then return (Value.return "configuration-button-active")
      else return (Value.return "configuration-button-inactive") in
    let%arr cls = cls
    and toggle_only_clocked = toggle_only_clocked in
    Vdom.Node.div ~attr:(Vdom.Attr.classes ["configuration-button"; cls]) [
      Vdom.Node.a  ~attr:(Vdom.Attr.on_click (fun _ -> toggle_only_clocked ()))
        [Vdom.Node.text "🕓"];
    ] in

  let%arr buffers = Value.map ~f:BufferList.buffers model
  and current_buffer = current_buffer_view
  and model = model 
  and set_state = set_state
  and search_bar = search_bar
  and clear_input = clear_input
  and only_clocked_button = only_clocked_button
  and hide_completed_button = hide_completed_button in
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
    let navigation_bar =
      navigation_bar_view
        (set_current_buffer set_state model)
        (fun () -> reload_state set_state)
        buffers in
    div ~attr:(Vdom.Attr.class_ "main") [
      navigation_bar;
      div ~attr:(Vdom.Attr.classes ["configuration-bar"; "centered-column"]) [
        div ~attr:(Vdom.Attr.class_ "configuration-buttons") [
          div ~attr:(Vdom.Attr.class_ "configuration-button") [
            a [text "↻"];
          ];
          only_clocked_button;
          hide_completed_button
        ];
        div ~attr:(Vdom.Attr.class_ "configuration-input") [
          search_bar;
          div ~attr:(Vdom.Attr.many_without_merge [
            Vdom.Attr.on_click (fun _ -> clear_input ());
            Vdom.Attr.class_ "configuration-button"
          ]) [
            a [text "⛌"];
          ];
        ];
      ];
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


