  $ ../sexp_parser.exe clock ../resources/clock.sexp
  (Ok Org_data.Clock {status = `running;
        value =
        { Org_data.raw = "[2022-11-22 Tue 07:10]";
          start =
          { Org_data.year = 2022; month = 11; day = 22; hour = (Some 7);
            minute = (Some 10) };
          end_ =
          { Org_data.year = 2022; month = 11; day = 22; hour = (Some 7);
            minute = (Some 10) };
          pos = { Org_data.begin_ = 203; end_ = 225 } };
        duration = None; pos = { Org_data.begin_ = 196; end_ = 226 }})
