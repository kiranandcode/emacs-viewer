open Core

let log = lazy (Dream.sub_log "state")

type buffer_data = {
  modification_time: Emacs_data.buffer_timestamp;
  buffer_name: string;
  buffer_filename: string;
  buffer_data: Emacs_data.t list
} [@@deriving sexp, eq, show]

type t = (string, buffer_data option) Hashtbl.t

let create () = Hashtbl.create (module String)

let get_buffer_data (t: t) buffer_filename modification_time =
  match Hashtbl.find t buffer_filename with
  | Some (Some data as cached_data) when
      Emacs_data.equal_buffer_timestamp
        data.modification_time modification_time ->
    cached_data
  | _ -> None

let set_buffer_data (t: t) buffer_filename buffer_data =
  Hashtbl.update_and_return t buffer_filename ~f:(function
    | Some (Some cached_data) when
        Emacs_data.buffer_timestamp_gt
          buffer_data.modification_time
          cached_data.modification_time ->
      (Some cached_data)
    | _ -> Some buffer_data
  )
  |> Option.value_exn ~here:[%here] ~message:"option should always be non-None here"


let set_buffer_list (t: t) ls =
  let ls = Hashtbl.of_alist_exn (module String) ls in
  Hashtbl.filter_mapi_inplace t ~f:(fun ~key ~data ->
    match Hashtbl.find ls key with
    | None ->
      (let lazy log = log in log).info (fun f -> f "dropping cached buffer %s" key);
      (* buffer is no longer present in emacs; drop it from the server *)
      None
    (* buffer exists *)
    | Some (_, timestamp) ->
      (* remove from ls *)
      Hashtbl.remove ls key;
      (* cached data *)
      match data with
      | Some old_data when
          Emacs_data.equal_buffer_timestamp
            old_data.modification_time timestamp ->
        (* if we have the latest modification time, then keep cached *)
        Some data
      | _ ->
        (* otherwise, clear cached, but keep entry *)
        Some None
  );
  Hashtbl.iteri ls ~f:(fun ~key:fname ~data:_ ->
    Hashtbl.add_exn t ~key:fname ~data:None
  )

let buffer_list (t: t) =
  let res = ref [] in
  Hashtbl.iteri t ~f:(fun ~key ~data ->
    let buffer_name = Option.map ~f:(fun data -> data.buffer_name) data in
    res := (key,buffer_name) :: !res);
  List.rev !res
