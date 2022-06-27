(lang dune 3.3)

(name c_lifetimes)

(generate_opam_files true)

(source
 (github gradual-verification/c_lifetimes))

(authors "Ian McCormack" "Timothy Zhou")

(maintainers "Ian McCormack" "Timothy Zhou")

(license LICENSE)

(documentation https://github.com/gradual-verification/c_lifetimes)

(package
 (name c_lifetimes)
 (synopsis "A lifetime analysis framework for C.")
 (description "A lifetime analysis framework for C, targeting an effectful extension of Rust's lifetime annotations for explicit memory management.")
 (depends ocaml dune goblint-cil)
)