(delete-file "scheme-rm.ss")

(load "rm-transformer.ss")
(compile-level-output)
(rm-transform-file "scheme-ds.ss" "scheme-rm.ss")
(exit)
