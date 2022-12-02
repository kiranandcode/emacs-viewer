  $ ../sexp_parser.exe --help
  SEXP_PARSER.EXE(1)          Sexp_parser.exe Manual          SEXP_PARSER.EXE(1)
  
  
  
  NNAAMMEE
         sexp_parser.exe
  
  SSYYNNOOPPSSIISS
         sseexxpp__ppaarrsseerr..eexxee [_O_P_T_I_O_N]… _T_Y_P_E _F_I_L_E
  
  AARRGGUUMMEENNTTSS
         _F_I_L_E (required)
             file containing sexp to parse
  
         _T_Y_P_E (required)
             type of value to parse
  
  CCOOMMMMOONN OOPPTTIIOONNSS
         ----hheellpp[=_F_M_T] (default=aauuttoo)
             Show this help in format _F_M_T. The value _F_M_T must be one of aauuttoo,
             ppaaggeerr, ggrrooffff or ppllaaiinn. With aauuttoo, the format is ppaaggeerr or ppllaaiinn
             whenever the TTEERRMM env var is dduummbb or undefined.
  
  EEXXIITT SSTTAATTUUSS
         sseexxpp__ppaarrsseerr..eexxee exits with the following status:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  
  
  Sexp_parser.exe                                             SEXP_PARSER.EXE(1)
