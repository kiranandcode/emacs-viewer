open Core

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

let rec subsection_contains_active_clock : Emacs_data.t -> bool =
  function
  | Emacs_data.Property _ -> false
  | Emacs_data.Section { pos=_; properties } ->
    List.exists ~f:subsection_contains_active_clock properties
  | Emacs_data.Headline _ -> false
  | Emacs_data.Drawer { name="LOGBOOK"; pos=_; contents } ->
    List.exists ~f:subsection_contains_active_clock contents
  | Emacs_data.Drawer { name=_; pos=_; contents=_ } -> false
  | Emacs_data.Clock {status;_} ->
    begin match status with `closed -> false | `running -> true end
  | Emacs_data.Planning _ -> false

let get_active_clock : Emacs_data.t -> _ option =
  let exception FoundClock of
      (Emacs_data.timestamp * string option * Emacs_data.pos) in
  let rec loop =
    function
    | Emacs_data.Property _ -> ()
    | Emacs_data.Section { pos=_; properties } ->
      List.iter ~f:loop properties
    | Emacs_data.Headline _ -> ()
    | Emacs_data.Drawer { name="LOGBOOK"; pos=_; contents } ->
      List.iter ~f:loop contents
    | Emacs_data.Drawer { name=_; pos=_; contents=_ } -> ()
    | Emacs_data.Clock {status; value; duration; pos} ->
      begin match status with `closed -> () | `running ->
        raise (FoundClock (value,duration,pos)) end
    | Emacs_data.Planning _ -> () in
  fun data -> try loop data; None with FoundClock (value, duration, pos) ->
    Some (value,duration,pos)

let rec subsection_contains_matching ~hide_completed ~only_clocked ~search_text ~filter_tags
  : Emacs_data.t -> bool =
  function
  | Emacs_data.Property _ -> false
  | Emacs_data.Section { pos=_; properties } ->
    List.exists ~f:(subsection_contains_matching
                      ~hide_completed ~only_clocked ~search_text ~filter_tags)
      properties
  | Emacs_data.Headline data ->
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
  | Emacs_data.Drawer _ 
  | Emacs_data.Clock _
  | Emacs_data.Planning _ -> false

let compare_times (ts1: Emacs_data.time) (ts2: Emacs_data.time) =
  let year_eq  = ts1.year  = ts2.year in
  let month_eq = ts1.month = ts2.month in
  let day_eq   = ts1.day   = ts2.day in
  let hours_eq = Option.equal Int.equal ts1.hour ts2.hour in
  let min_eq = Option.equal Int.equal ts1.minute ts2.minute in
  match year_eq, month_eq, day_eq with
  | true, true, true -> begin
      if hours_eq && min_eq
      then `EqualDates
      else `SameDatesDifferentTimes
    end
  | _ -> `DifferentDates
