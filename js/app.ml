[@@@warning "-32"]
open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module Buffer = struct

  type t = {
    timestamp: Emacs_data.buffer_timestamp;
    buffer_filename: string;
    elements: Emacs_data.t list
  } [@@deriving sexp, fields, equal]

  let init_opt data = match data with
    | Ok (filename, data, timestamp) -> Some {buffer_filename=filename;elements=data;timestamp}
    | Error _ -> None

end

module BufferList = struct

  type t = {
    buffers: (string * string option) list;
    current_buffer: Buffer.t option;
  } [@@deriving sexp, fields, equal]

  let init_opt ?current_buffer data =
    match data with
    | Ok data -> Some {buffers=data; current_buffer}
    | Error _ -> None

  let set_current_buffer t buffer =
    {t with current_buffer=Some buffer}

end

let loading_view =
  let open Vdom.Node in
  Value.return @@ div ~attr:Vdom.Attr.(many_without_merge [
    classes ["hero"; "main-info"; "loading-info"]
  ]) [
    div ~attr:(Vdom.Attr.class_ "loading-panel") [
      div ~attr:(Vdom.Attr.class_ "loader") [];
      div ~attr:(Vdom.Attr.class_ "info-loading") [
        text "Loading...";
      ];
    ]
  ]

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

let render_time (ts: Emacs_data.time) =
  let time = Format.sprintf "%02d-%02d-%02d%s" ts.year ts.month ts.day
               (match ts.hour, ts.minute with
                | Some hour, Some minute -> Format.sprintf " %02d:%02d" hour minute
                | Some hour, None -> Format.sprintf " %02d:00" hour
                | None, Some minute -> Format.sprintf " 00:%02d" minute
                | None, None -> "") in
  Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-time") Vdom.Node.[p [text time]]

let render_timestamp (ts: Emacs_data.timestamp) =
  match Utils.compare_times ts.start ts.end_ with
  | `EqualDates ->
    Vdom.Node.div ~attr:(Vdom.Attr.classes ["org-mode-timestamp"; "org-mode-timestamp-single"]) [ render_time ts.start ]
  | `SameDatesDifferentTimes ->
    Vdom.Node.div ~attr:(Vdom.Attr.classes ["org-mode-timestamp"; "org-mode-timestamp-time-range"]) [
      render_time Emacs_data.{ts.start with hour=None; minute=None};
      let (!) v = Option.value ~default:0 v in
      Vdom.Node.div ~attr:(Vdom.Attr.classes ["org-time";"org-time-diff"]) [
        Vdom.Node.(p [text (Format.sprintf "%02d:%02d" (! (ts.start.hour)) (! (ts.start.minute)))]);
        Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-timestamp-separator") [ Vdom.Node.text "--" ];
        Vdom.Node.(p [text (Format.sprintf "%02d:%02d" (! (ts.end_.hour)) (! (ts.end_.minute)))]);
      ]
    ]
  | `DifferentDates ->
    Vdom.Node.div ~attr:(Vdom.Attr.classes ["org-mode-timestamp"; "org-mode-timestamp-multi"]) [
      render_time ts.start;
      Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-timestamp-separator") [ Vdom.Node.text "--" ];
      render_time ts.end_;
    ]

let rec render_text (txt: Emacs_data.txt) =
  let to_class = function
    | Emacs_data.Italic -> "italic"
    | Emacs_data.Strikethrough -> "strikethrough" | Emacs_data.Bold -> "bold"
    | Emacs_data.Underline -> "underline" | Emacs_data.Subscript -> "subscript"
    | Emacs_data.Superscript -> "superscript" in
  let open Vdom.Node in
  match txt with
  | Emacs_data.Lit txt -> p [text txt]
  | Emacs_data.Timestamp ts -> render_timestamp ts
  | Emacs_data.Concat elts ->
    let elts = List.map ~f:render_text elts in
    div elts
  | Emacs_data.StatisticsCookie txt ->
    p ~attr:(Vdom.Attr.class_ "org-text-stats-cookie") [text txt]
  | Emacs_data.Code cde ->
    pre [text cde]
  | Emacs_data.Verbatim verb ->
    pre [text verb]
  | Emacs_data.Entity ent ->
    p ~attr:(Vdom.Attr.class_ "org-text-entity") [text ent]
  | Emacs_data.Format (fmt, t) ->
    div ~attr:(Vdom.Attr.class_ (to_class fmt)) [render_text t]
  | Emacs_data.InlineSrcBlock { language; value } ->
    pre [code ~attr:(Vdom.Attr.class_ ("language-" ^ language)) [text value]]

let render_todo ({ keyword; ty }: Emacs_data.todo) =
  let todo_class = match ty with `Done -> "org-mode-todo-done" | `Todo -> "org-mode-todo-todo" in
  Vdom.Node.div ~attr:(Vdom.Attr.classes ["org-mode-todo"; todo_class]) [ Vdom.Node.text keyword ]

let rec data_to_view ~hide_completed ~only_clocked  ~search_text ~filter_tags ~add_tag
      ~open_in_emacs ~change_clock_status ~change_todo_status :
  Emacs_data.t Value.t -> Vdom.Node.t Computation.t = fun data ->
  match%sub data with
  | Emacs_data.Property { key; value; pos=_ } ->
    let%arr key = key
    and value = value in
    Vdom.Node.(
      div ~attr:(Vdom.Attr.classes ["org-data"; "org-property"]) [
        div ~attr:(Vdom.Attr.classes ["org-property-key"]) [ text key ];
        div ~attr:(Vdom.Attr.classes ["org-property-value"]) [ text value ];
      ]
    )
  | Emacs_data.Section { pos=_; properties } ->
    let%sub children = Bonsai.lazy_ @@
      lazy (mapM ~f:(data_to_view  ~only_clocked ~hide_completed ~search_text ~filter_tags ~add_tag
                       ~open_in_emacs ~change_clock_status ~change_todo_status) properties) in
    let%arr children = children in
    Vdom.Node.div ~attr:(Vdom.Attr.classes ["org-mode-section"; "org-mode-children"]) children
  | Emacs_data.Headline {
    title;
    raw_value;
    pos; level;
    priority=_; 
    tags;
    todo;
    subsections;
    closed=(_: Emacs_data.timestamp option);
    scheduled=(_: Emacs_data.timestamp option);
    deadline=(_: Emacs_data.timestamp option);
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
      List.exists ~f:Utils.subsection_contains_active_clock subsections in
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
      List.exists ~f:(Utils.subsection_contains_matching ~only_clocked ~hide_completed
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
          match List.find_map ~f:Utils.get_active_clock subsections with
          | None -> None
          | Some (time,_duration,_pos) ->
            Option.map ~f:(fun clock_start ->
              Time_ns.diff current_time clock_start
              |> Time_ns.Span.to_short_string
            ) @@
            Option.first_some
              (Emacs_data.time_to_time_ns time.start)
              (Emacs_data.time_to_time_ns time.end_) 
        else return (Value.return None) in
      let%sub fold_buttons_panel =
        let%sub fold_button =
          if%sub should_fold
          then return (Value.return "˅")
          else return (Value.return "᠁") in
        let%sub fold_button_class =
          if%sub should_fold
          then return (Value.return "fold-action-folded")
          else return (Value.return "fold-action-unfolded") in
        let%arr subsections = subsections
        and toggle_fold = set_should_fold
        and fold_button = fold_button
        and fold_button_class = fold_button_class in
        if List.is_empty subsections
        then []
        else [
          Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-section-fold-buttons") [
            Vdom.Node.div ~attr:(Vdom.Attr.many_without_merge [
              Vdom.Attr.on_click (fun _ -> toggle_fold ());
              Vdom.Attr.classes ["org-mode-section-button"; fold_button_class]
            ]) [
              Vdom.Node.(a [text fold_button]);
            ];
          ]] in
      let subsections = Bonsai.lazy_ @@
        lazy (mapM ~f:(data_to_view
                         ~only_clocked ~hide_completed
                         ~search_text ~filter_tags ~add_tag
                         ~open_in_emacs ~change_clock_status ~change_todo_status) subsections) in
      let%sub subsections =
        if%sub should_fold then return (Value.return []) else subsections in
      let%arr subsections = subsections
      and title = title
      and level = level
      and tags = tags
      and todo = todo
      and add_tag = add_tag
      and clock_run_time = clock_run_time
      and fold_buttons_panel = fold_buttons_panel
      and open_in_emacs = open_in_emacs
      and change_clock_status = change_clock_status
      and change_todo_status = change_todo_status
      and pos = pos in

      let section_button effect icon =
        Vdom.Node.div ~attr:(Vdom.Attr.many_without_merge [
          Vdom.Attr.class_ "org-mode-section-button";
          Vdom.Attr.on_click effect;
        ]) [ Vdom.Node.(a [text icon]); ] in

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
                  [Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-title-todo") [render_todo todo]]);
               [Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-title-text") [render_text title]];
               (match clock_run_time with
                | None -> []
                | Some run_time ->
                  [Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-title-clock") [
                     Vdom.Node.div [ Vdom.Node.text run_time ]
                   ]]);
               [Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-section-buttons") ([
                  Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-section-action-buttons") [
                    section_button (fun _ -> open_in_emacs pos.begin_) "📂";
                    section_button (fun _ -> change_clock_status pos.begin_ (Option.is_none clock_run_time)) "🕓";
                    section_button (fun _ -> change_todo_status pos.begin_) "🞋";
                  ];
                ] @ fold_buttons_panel)]
             ]);
          Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-section-details") [
            Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-tags") (List.map ~f:(fun v ->
              Vdom.Node.(p ~attr:(Vdom.Attr.on_click (fun _ -> add_tag v)) [text v])) tags
            );
          ]
        ];
        Vdom.Node.div ~attr:(Vdom.Attr.classes ["org-mode-headline-childen";"org-mode-children"]) subsections
      ]
    end
    else return (Value.return (Vdom.Node.text ""))
  | Emacs_data.Drawer { name; pos=_; contents } ->
    let%sub contents = Bonsai.lazy_ @@
      lazy (mapM ~f:(data_to_view ~only_clocked ~hide_completed ~search_text ~filter_tags ~add_tag
                    ~open_in_emacs ~change_clock_status ~change_todo_status) contents) in
    let%arr contents = contents
    and name = name in
    Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-drawer") [
      Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-drawer-name") [Vdom.Node.text name];
      Vdom.Node.div ~attr:(Vdom.Attr.classes ["org-mode-drawer-children";"org-mode-children"]) contents;
    ]
  | Emacs_data.Clock {status; value;duration; pos=_} ->
    let%arr status = status
    and value = value
    and duration = duration in
    let clock_class = match status with
      | `closed -> "org-mode-clock-closed"
      | `running -> "org-mode-clock-running" in
    Vdom.Node.div ~attr:(Vdom.Attr.classes ["org-mode-clock";clock_class]) [
      Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-clock-status") [
        Vdom.Node.text (match status with
          | `closed -> "closed"
          | `running -> "running"
        )];
      Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-clock-duration")
        (match duration with
           None -> []
         | Some duration ->
           [Vdom.Node.text duration]);
      Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-clock-value")
        [(render_timestamp value)];
    ]
  | Emacs_data.Planning {
    closed; scheduled; deadline; pos=_
  } ->
    let%arr closed = closed
    and scheduled = scheduled
    and deadline = deadline in
    Vdom.Node.div ~attr:(Vdom.Attr.class_ "org-mode-planning") (List.concat [
      (match closed with None -> [] | Some closed ->
         [Vdom.Node.div ~attr:(Vdom.Attr.classes ["org-mode-planning-timestamp"; "org-mode-planning-closed"])
            [render_timestamp closed]]);
      (match scheduled with None -> [] | Some scheduled ->
         [Vdom.Node.div ~attr:(Vdom.Attr.classes ["org-mode-planning-timestamp"; "org-mode-planning-scheduled"])
            [render_timestamp scheduled]]);
      (match deadline with None -> [] | Some deadline ->
         [Vdom.Node.div ~attr:(Vdom.Attr.classes ["org-mode-planning-timestamp"; "org-mode-planning-deadline"])
            [render_timestamp deadline]]);
    ])

let current_buffer_to_view
      ~hide_completed ~only_clocked ~search_text ~filter_tags ~add_tag
      ~open_in_emacs ~change_clock_status ~change_todo_status current_buffer =
  match%sub current_buffer with
  | None ->
    return @@ (Value.return @@
               Vdom.Node.div
                 ~attr:(Vdom.Attr.classes
                          ["org-mode-current-buffer";
                           "org-mode-empty-buffer";
                           "centered-column"])
                 [Vdom.Node.text "Please select a buffer..."])
  | Some buffer ->
    let%sub buffer_entries =
      mapM ~f:(data_to_view ~only_clocked ~hide_completed ~search_text ~filter_tags ~add_tag
                 ~open_in_emacs ~change_clock_status ~change_todo_status)
        (Value.map ~f:Buffer.elements buffer) in
    let%arr buffer_entries = buffer_entries in
    Vdom.Node.div ~attr:(Vdom.Attr.classes ["org-mode-current-buffer"; "centered-column"])
      buffer_entries

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

let bufferlist_to_view ~set_current_buffer ~set_state ~reload_state
      ~reload_current_buffer ~run_action
      (model: BufferList.t Value.t) : Vdom.Node.t Computation.t =
  let%sub current_buffer = return (Value.map ~f:BufferList.current_buffer model) in
  let%sub (search_text, tags, clear_input, add_tag), search_bar = Components.tagged_search_bar in
  let%sub hide_completed, toggle_hide_completed =
    Bonsai.state_machine0 [%here] (module Bool) (module Unit) ~default_model:false
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model () -> not model) in
  let%sub only_clocked, toggle_only_clocked =
    Bonsai.state_machine0 [%here] (module Bool) (module Unit) ~default_model:false
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model () -> not model) in
  let%sub open_in_emacs =
    let%arr model = model
    and set_state = set_state in
    match model.current_buffer with
    | None -> fun _ -> Ui_effect.return ()
    | Some buffer ->
      fun pos ->
        let%bind.Effect res = run_action (buffer.buffer_filename, buffer.timestamp, pos, `open_in_emacs) in
        match res with
        | Error _ -> Ui_effect.return ()
        | Ok (Some ()) -> Ui_effect.return ()
        | _ -> reload_current_buffer set_state model in
  let%sub change_clock_status =
    let%arr model = model
    and set_state = set_state in
    match model.current_buffer with
    | None -> fun _ _ -> Ui_effect.return ()
    | Some buffer ->
      fun pos active ->
        let action = if active then `clock_in else `clock_out in
        let%bind.Effect res = run_action (buffer.buffer_filename, buffer.timestamp, pos, action) in
        match res with
        | Error _ -> Ui_effect.return ()
        | Ok _ ->
          reload_current_buffer set_state model in
  let%sub change_todo_status =
    let%arr model = model
    and set_state = set_state in
    match model.current_buffer with
    | None -> fun _ -> Ui_effect.return ()
    | Some buffer ->
      fun pos ->
        let%bind.Effect res = run_action (buffer.buffer_filename, buffer.timestamp, pos, `change_todo) in
        match res with
        | Error _ -> Ui_effect.return ()
        | Ok _ ->
          reload_current_buffer set_state model in
  let%sub current_buffer_view =
    current_buffer_to_view ~hide_completed ~only_clocked
      ~search_text ~filter_tags:tags ~add_tag
      ~open_in_emacs ~change_clock_status ~change_todo_status current_buffer in

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

  let%sub reload_buffer_action =
    let%arr model = model and set_state = set_state in
    reload_current_buffer set_state model in
  let%sub () = Bonsai.Clock.every [%here] (Time_ns.Span.of_sec 5.) reload_buffer_action in
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
        (fun () -> reload_state set_state model)
        buffers in
    div ~attr:(Vdom.Attr.class_ "main") [
      navigation_bar;
      div ~attr:(Vdom.Attr.classes ["configuration-bar"; "centered-column"]) [
        div ~attr:(Vdom.Attr.class_ "configuration-buttons") [
          div ~attr:(Vdom.Attr.class_ "configuration-button") [
            a ~attr:(Vdom.Attr.on_click (fun _ ->
              reload_current_buffer set_state model
            )) [text "↻"];
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

let view =
  let%sub state, set_state = Bonsai.state_opt [%here] (module BufferList) in
  let set_current_buffer set_state state name =
    let%bind.Effect buffer = Actions.get_buffer name in
    match Buffer.init_opt buffer with
    | Some buffer ->
      let state = BufferList.set_current_buffer state buffer in
      set_state (Some state)
    | None -> Ui_effect.return () in
  let reload_current_buffer set_state buffer_list =
    match buffer_list.BufferList.current_buffer with
    | None -> Ui_effect.return ()
    | Some current_buffer ->
      let buffer_name = current_buffer.Buffer.buffer_filename in
      let buffer_timestamp = current_buffer.Buffer.timestamp in
      let%bind.Effect data = Actions.reload_buffer (buffer_name, buffer_timestamp) in
      match Result.map ~f:(Option.bind ~f:(fun (d,t) -> Buffer.init_opt (Ok (buffer_name,d,t)))) data with
      | Ok (Some current_buffer) ->
        let state = BufferList.set_current_buffer buffer_list current_buffer in
        set_state (Some state)
      | _ -> Ui_effect.return () in
  let reload_state set_state buffer_list =
    let%bind.Effect data = Actions.reload_buffer_list
                             (Option.map ~f:Buffer.buffer_filename buffer_list.BufferList.current_buffer) in
    match data with
    | Ok (buffer_list, current_buffer) ->
      let current_buffer = Buffer.init_opt (Result.of_option ~error:() current_buffer) in
      set_state (BufferList.init_opt ?current_buffer (Ok buffer_list))
    | Error _ ->
      Ui_effect.return () in
  let%sub on_activate =
    let%arr set_state = set_state in
    let%bind.Effect data = Actions.get_buffer_list () in
    set_state (BufferList.init_opt data) in
  let%sub () = Bonsai.Edge.lifecycle ~on_activate () in
  match%sub state with
  | None ->
    return loading_view
  | Some model ->
    let%sub buffer_list =
      bufferlist_to_view ~set_current_buffer ~reload_current_buffer ~reload_state ~set_state
        ~run_action:Actions.run_action model in
    return buffer_list

let application =
  let%sub view = view in
  let%arr buffers = view in
  Vdom.Node.div [buffers]

let (_: _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app"
    application
