(lang dune 3.4)

(name emacs_viewer)

(generate_opam_files true)
(cram enable)

(authors "gopiandcode")

(maintainers "gopiandcode")

(license AGPL-3.0-or-later)

(homepage "https://codeberg.org/gopiandcode/emacs-viewer")
(bug_reports "https://codeberg.org/gopiandcode/emacs-viewer/issues")

(package
 (name emacs_viewer)
 (synopsis "Web viewer integration for Emacs")
 (description "Web viewer integration for Emacs")
 (depends
   ocaml dune
   core re sexplib
   dream cmdliner
   decoders decoders-sexplib
   bonsai fuzzy_match timezone
   async_js 
   ppx_here ppx_compare ppx_let ppx_sexp_conv ppx_fields_conv ppx_deriving ppx_blob
   bos)
 (depopts
   inotify core_unix ptime)
 (tags
  (emacs web view)))
