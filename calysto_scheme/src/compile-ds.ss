(load "ds-transformer.ss")
(delete-file "source-ds.ss")
(ds-transform-file "source-cps.ss" "source-ds.ss")
(exit)
