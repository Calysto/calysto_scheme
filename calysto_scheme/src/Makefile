SCHEME=scheme

all:  ../scheme.py

../scheme.py: scheme-rm.ss translate_rm.py Scheme.py Makefile
	python translate_rm.py "scheme-rm.ss" "../scheme.py"

scheme-ds.ss: reader-cps.ss environments-cps.ss parser-cps.ss \
		interpreter-cps.ss unifier-cps.ss ds-transformer.ss
	cat reader-cps.ss environments-cps.ss parser-cps.ss interpreter-cps.ss unifier-cps.ss > scheme-cps.ss
	echo "(load \"ds-transformer.ss\")" > compile-ds.ss
	echo "(delete-file \"scheme-ds.ss\")" >> compile-ds.ss
	echo "(ds-transform-file \"scheme-cps.ss\" \"scheme-ds.ss\")" >> compile-ds.ss
	echo "(exit)" >> compile-ds.ss
	$(SCHEME) compile-ds.ss

scheme-rm.ss: scheme-ds.ss rm-transformer.ss
	echo "(load \"rm-transformer.ss\")" > compile-rm.ss
	echo "(delete-file \"scheme-rm.ss\")" >> compile-rm.ss
	echo "(compile-level-output)" >> compile-rm.ss
	echo "(rm-transform-file \"scheme-ds.ss\" \"scheme-rm.ss\")" >> compile-rm.ss
	echo "(exit)" >> compile-rm.ss
	$(SCHEME) compile-rm.ss

run: scheme-rm.ss 
	echo "(case-sensitive #t)" > scheme-run.ss
	echo "(load \"scheme-rm.ss\")" >> scheme-run.ss
	echo "(load \"no-csharp-support.ss\")" >> scheme-run.ss
	echo "(begin (display (run start)) (newline))" >> scheme-run.ss
	$(SCHEME) scheme-run.ss

clean:
	$(RM) *.exe *~ scheme-ds.ss scheme-rm.ss compile-rm.ss compile-ds.ss make-fact.ss \
		scheme-cps.ss \
		scheme-run.ss scheme.ss all*.ss 

