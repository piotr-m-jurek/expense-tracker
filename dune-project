(lang dune 3.17)

(name expense_tracker)

(generate_opam_files true)

(source
 (github piotr-m-jurek/expense-tracker))

(authors "Piotr Malecki-Jurek <piotr.m.jurek@proton.me>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name expense_tracker)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml core csv ocamlformat core_unix ppx_jane)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
