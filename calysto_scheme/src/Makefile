SCHEME=scheme
PYTHON=python

all: ../scheme.py scheme-cps.ss scheme-ds.ss scheme-rm.ss

../scheme.py: source-rm.ss translate_rm.py Scheme.py
	$(PYTHON) translate_rm.py "source-rm.ss" "../scheme.py"

source-rm.ss: source-ds.ss compile-rm.ss
	$(SCHEME) compile-rm.ss

source-ds.ss: source-cps.ss compile-ds.ss
	$(SCHEME) compile-ds.ss

source-cps.ss: reader-cps.ss environments-cps.ss parser-cps.ss interpreter-cps.ss unifier-cps.ss
	cat reader-cps.ss environments-cps.ss parser-cps.ss interpreter-cps.ss unifier-cps.ss > source-cps.ss

scheme-cps.ss: source-cps.ss
	echo "(load \"source-cps.ss\")" > scheme-cps.ss
	echo "(start)" >> scheme-cps.ss

scheme-ds.ss: source-ds.ss
	echo "(load \"source-ds.ss\")" > scheme-ds.ss
	echo "(start)" >> scheme-ds.ss

scheme-rm.ss: source-rm.ss
	echo "(load \"source-rm.ss\")" > scheme-rm.ss
	echo "(start)" >> scheme-rm.ss

clean:
	$(RM) -f source-*.ss scheme-*.ss ../scheme.py *~
