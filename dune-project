(lang dune 2.7)
(name ldap-tools)
(version 0.1.0)
(generate_opam_files true)
(license MIT)
(authors "Benjamin Somers <bsomers@resel.fr>")
(maintainers "Benjamin Somers <bsomers@resel.fr>")

(package
 (name ldap-tools)
 (synopsis "Simplify LDAP interactions")
 (description "This package provides tools to simplify interactions with LDAP servers")
 (depends
  (alcotest (>= 1.6.0))
  ptime
  (ppxlib (>= 0.27.0))))
