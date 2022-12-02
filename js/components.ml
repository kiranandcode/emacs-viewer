[@@@warning "-32"]
open! Core
open! Bonsai_web
open Bonsai.Let_syntax


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
  let%sub text, update_text =
    Bonsai.state_machine1 [%here]
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
