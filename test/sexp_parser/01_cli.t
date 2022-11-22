  $ ../sexp_parser.exe --help
  NAME
         sexp_parser.exe
  
  SYNOPSIS
         sexp_parser.exe [OPTION]… TYPE FILE
  
  ARGUMENTS
         FILE (required)
             file containing sexp to parse
  
         TYPE (required)
             type of value to parse
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         sexp_parser.exe exits with the following status:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
