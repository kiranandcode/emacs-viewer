# Scripts

Defines a helper binary for rebuilding files. This could have been a
script in bash (and even was at some point), but writing it in OCaml
made certain things easier.

To use it:
```
dune exec ./scripts/run_and_rebuild.exe
```
Then make any change in the source files to trigger a rebuild.
