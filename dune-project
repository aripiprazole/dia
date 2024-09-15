(lang dune 3.16)

(name dia)

(generate_opam_files true)

(source
 (github aripiprazole/dia))

(authors "Gabrielle Guimarães de Oliveira")

(maintainers "Gabrielle Guimarães de Oliveira")

(license LICENSE)

(documentation https://github.com/aripiprazole/dia)

(package
 (name dia)
 (synopsis "A cubical type theory theorem prover")
 (description
  "A thereom prover with HITs, homotopy features and cubical type theory")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

(using menhir 2.1)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
