(lang dune 2.9)
(name dts_of_ocaml)

(generate_opam_files true)

(source (github ayazhafiz/dts_of_ocaml))
(license MIT)
(authors "Ayaz Hafiz")
(maintainers "ayaz.hafiz.1@gmail.com")

(package
 (name dts_of_ocaml)
 (synopsis "Generates TypeScript declarations from js_of_ocaml sources.")
 (description "\
dts_of_ocaml is a generator of TypeScript declarations (.d.ts files) from OCaml
sources fed to js_of_ocaml. For an OCaml source file `js.ml` that is compiled
with js_of_ocaml, feeding `js.ml`'s `js.cmt` to dts_of_ocaml will generate
TypeScript declarations of module exports.
")
 (depends
  compiler-libs))
