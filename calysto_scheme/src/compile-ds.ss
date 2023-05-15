(delete-file "scheme-ds.ss")

(load "ds-transformer.ss")
(ds-transform-file "scheme-cps.ss" "scheme-ds.ss")
(exit)
