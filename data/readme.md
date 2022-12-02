# Data
This file defines a generic internal representation of Org-mode data in OCaml.

```
type t =
  | Property of property
  | Section of { pos: pos; properties: t list}
  ...
```

The definitions in this file are shared between the backend and
frontend, using sexp-based serialisation/deserialisation to pass
information between them.
