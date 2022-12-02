(lang dune 3.4)

(name emacs_viewer)

(generate_opam_files true)
(cram enable)

(authors "gopiandcode")

(maintainers "gopiandcode")

(license AGPLv3.0+)

(package
 (name emacs_viewer)
 (synopsis "Web viewer integration for Emacs")
 (description "Web viewer integration for Emacs")
 (depends
   ocaml
   core
   dune
   dream
   unix
 )
 (tags
  (topics emacs web view)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
