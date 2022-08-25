(lang dune 3.3)

(name lifeline)

(generate_opam_files true)

(source
 (github gradual-verification/lifeline))

(authors "Ian McCormack" "Timothy Zhou")

(maintainers "Ian McCormack" "Timothy Zhou")

(license LICENSE)

(documentation https://github.com/gradual-verification/lifeline)

(package
 (name lifeline)
 (synopsis "A lifetime analysis framework for C.")
 (description "A lifetime analysis framework for C, targeting an effectful extension of Rust's lifetime annotations for explicit memory management.")
 (depends ocaml dune goblint-cil core)
)