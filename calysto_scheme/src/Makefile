SCHEME=scheme
PYTHON=python

../scheme.py: scheme-cps.ss compile-ds.ss compile-rm.ss translate_rm.py
	$(SCHEME) compile-ds.ss
	$(SCHEME) compile-rm.ss
	$(PYTHON) translate_rm.py "scheme-rm.ss" "../scheme.py"

scheme-cps.ss: reader-cps.ss environments-cps.ss parser-cps.ss interpreter-cps.ss unifier-cps.ss
	cat reader-cps.ss environments-cps.ss parser-cps.ss interpreter-cps.ss unifier-cps.ss > scheme-cps.ss

clean:
	$(RM) -f scheme-cps.ss ../scheme.py *~
