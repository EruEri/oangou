(lang dune 3.10)

(name oangou)

(generate_opam_files true)

(version "0.1.0")

(source
 (github EruEri/oangou))

(authors "EruEri <ndiayeyves@gmail.com>")

(maintainers "EruEri <ndiayeyves@gmail.com>")

(license GPL-3.0-or-later)

; (documentation https://url/to/documentation)

(package
 (name oangou)
 (synopsis "A content encryptor")
 (description "Oangou is a encryption tool. \
 It allows to encrypt content and share it with a peer. \
 It uses the Elliptic curve Diffie–Hellman to generate a public/private keys \
 and the generated shared secret with the public keys of the peer")
 (depends (ocaml (>= "4.14.0")) dune xdg (cmdliner (>= "1.1.0"))  (ppx_deriving_yojson (>= "3.5.3")) (yojson (>= "2.0.0")) (cmdliner (>= "1.1.0")) mirage-crypto-ec)
 (tags (encryption)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
