(lang dune 3.7)

(name aoc)
(version 0.1.0)

(generate_opam_files true)

(source
 (github baffalop/aocaml))

(authors "Sixstring982", "Baffalop")

(maintainers "Sixstring982" "Baffalop")

(license BSD-3-Clause)

(documentation https://sixstring982.github.io/tanenbaum)

(package
 (name aoc)
 (synopsis "Advent of Code solutions in OCaml, using Tanenbaum starter pack")
 (depends
  ocaml
  reason
  dune
  (cmdliner(>= 1.2.0))
  (piaf (>= 0.2.0))
  (fmt (>= 0.9.0))
  angstrom
  )
 (tags ("advent-of-code")))
