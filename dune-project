(lang dune 3.17)

(name oasis)

(generate_opam_files true)

(source
 (github CharlesAverill/oasis))

(authors "Charles Averill")

(maintainers "Charles Averill")

(license MIT)

(documentation https://github.com/CharlesAverill/oasis)

(package
 (name oasis)
 (synopsis "OCaml needs Another Standard lIbrary")
 (description "Another standard library for ocaml, just for fun")
 (depends
  (ocaml
   (>= 5.3.0))
  (dune
   (>= 3.17))
  (alcotest :with-test)
  (ocamlformat :with-test)
  (ocamlformat :with-doc)
  (odoc :with-doc)
  (sherlodoc :with-doc)))
