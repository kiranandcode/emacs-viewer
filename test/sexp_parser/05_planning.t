  $ ../sexp_parser.exe planning ../resources/planning.sexp
  (Ok Org_data.Planning {
        closed =
        (Some { Org_data.raw = "[2022-11-22 Tue 06:58]";
                start =
                { Org_data.year = 2022; month = 11; day = 22; hour = (Some 6);
                  minute = (Some 58) };
                end_ =
                { Org_data.year = 2022; month = 11; day = 22; hour = (Some 6);
                  minute = (Some 58) };
                pos = { Org_data.begin_ = 559; end_ = 581 } });
        scheduled = None; deadline = None;
        pos = { Org_data.begin_ = 551; end_ = 582 }})
