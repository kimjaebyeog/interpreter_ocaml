(lang dune 3.15)

(name interpreter)

(generate_opam_files true)

; (wrapped_executables false) 

(map_workspace_root false) ; to use ocaml earlybird debugger

; (source
;  (github username/reponame))

(authors "Jaebyeog Kim <jaebyeogkim@gmail.com>")

(maintainers "Jaebyeog Kim <jaebyeogkim@gmail.com>")

(license MIT)

; (documentation https://url/to/documentation)

(using menhir 3.0)

(package
 (name interpreter)
 (synopsis "An interpreter for a functional language")
;  (description "A longer description")
 (depends 
  (ocaml (>= 4.13.1))
  dune
  menhir )
 (tags
  ("interpreter" "functional language" "type inference"))
)

; ; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
