(lang dune 3.17)

(name bech32)

(generate_opam_files true)

(source
 (github vbmithr/ocaml-bech32))

(authors "Vincent Bernardoff <vb@luminar.eu.org>")

(maintainers "Vincent Bernardoff")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name bech32)
 (synopsis "Bech32 implementation")
 (description "This package implements parsing and pretty-printing of Bech32 addresses, used in Bitcoin as well as other blockchains projects, i.e. Zilliqa.")
 (depends
   ocaml
   dune
   base
  (hex :with-test)
  (alcotest :with-test)
 )
 (tags (crypto bitcoin bech32)))
