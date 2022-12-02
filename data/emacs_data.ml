open Core

type pos = {
  begin_: int;
  end_: int;
}
[@@deriving show, sexp, equal]

type property = { key: string; value: string; pos: pos}
[@@deriving show, sexp, equal]

type tag = string
[@@deriving show, sexp, equal]

type op = Italic | Strikethrough | Bold | Underline | Subscript | Superscript
[@@deriving show, sexp, equal]

type time = { year: int; month: int; day: int; hour: int option; minute: int option }
[@@deriving show, sexp, equal]

type timestamp = { raw: string; start: time; end_: time; pos: pos }
[@@deriving show, sexp, equal]

let time_to_time_ns ts =
  let (let+) x f = Option.bind x ~f in
  let+ month = Month.of_int ts.month in
  let+ date = try
      Some (Date.create_exn ~y:ts.year ~m:month ~d:ts.day)
    with Invalid_argument _ -> None in
  let time = Time_ns.Ofday.create
               ~hr:(Option.value ~default:0 ts.hour)
               ~min:(Option.value ~default:0 ts.minute) () in
  Some (Time_ns.of_date_ofday
          ~zone:(Timezone.utc)
          date
          time)

type txt =
  | Lit of string
  | Concat of txt list
  | Code of string | Verbatim of string
  | Entity of string
  | Format of op * txt
  | InlineSrcBlock of { language: string; value: string }  
  | Timestamp of timestamp
  | StatisticsCookie of string
[@@deriving show, sexp, equal]


type todo = {
  keyword: string;
  ty: [`Todo | `Done];
}
[@@deriving show, sexp, equal]

type t =
  | Property of property
  | Section of { pos: pos; properties: t list}
  | Headline of {
      raw_value: string;
      title: txt;
      pos: pos;
      level: int;
      priority: int option;
      tags: tag list;
      todo: todo option;
      subsections: t list;
      closed: timestamp option;
      scheduled: timestamp option;
      deadline: timestamp option;
    }
  | Drawer of { name: string; pos: pos; contents: t list }
  | Clock of {
      status: [`running | `closed ];
      value: timestamp;
      duration: string option;
      pos: pos;
    }
  | Planning of {
      closed: timestamp option;
      scheduled: timestamp option;
      deadline: timestamp option;
      pos: pos;
    }
[@@deriving show, sexp, equal]

type buffer_timestamp = { modification_time: int; modification_count: int }
[@@deriving show, sexp, equal]

let buffer_timestamp_gt t1 t2 =
  t1.modification_time < t2.modification_time || (
    t1.modification_time = t2.modification_time &&
    t1.modification_count < t2.modification_count
  )
