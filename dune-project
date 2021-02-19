(lang dune 2.7)
(wrapped_executables true)
(generate_opam_files true)
(name ocaml_nanogui)
(package
    (name ocaml_nanogui)
    (synopsis "A small UI framework using OpenGL written in OCaml")
    (description "A small UI framework using OpenGL written in OCaml")
)

(package
    (name ocaml_nanogui.async)
    (synopsis "Async support for nano gui")
    (description "Async_kernel support for nano gui")
)

(package
    (name ocaml_nanogui.libext)
    (synopsis "Small extension of Stdlib")
    (description "A small extension to Stdlib")
)
