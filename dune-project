(lang dune 3.0)

(name chasity)

(generate_opam_files true)

(package
 (name chasity)
 (synopsis "SHACL to Protobuf transpiler")
 (depends
  (ocaml (>= 5.1))
  cmdliner
  fmt))
