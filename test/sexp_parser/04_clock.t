  $ ../sexp_parser.exe clock ../resources/clock.sexp
  (Ok Emacs_data.Clock {status = `running;
        value =
        { Emacs_data.raw = "[2022-11-22 Tue 07:10]";
          start =
          { Emacs_data.year = 2022; month = 11; day = 22; hour = (Some 7);
            minute = (Some 10) };
          end_ =
          { Emacs_data.year = 2022; month = 11; day = 22; hour = (Some 7);
            minute = (Some 10) };
          pos = { Emacs_data.begin_ = 203; end_ = 225 } };
        duration = None; pos = { Emacs_data.begin_ = 196; end_ = 226 }})
