The source code for Calysto Scheme is in [calysto_scheme/src/](https://github.com/Calysto/calysto_scheme/tree/master/calysto_scheme/src). Calysto Scheme is written in Scheme and then converted into Python (and possibly other implementation languages, including C#).

The following files make up Calysto Scheme system:

* interpreter-cps.ss
* environment-cps.ss
* parser-cps.ss
* reader-cps.ss
* unifier-cps.ss

As the names imply, they are written in Continuation-Passing Style (CPS). There is one additional file needed for Python: 

* Scheme.py

This contains the Python-specific definitions, and will be included in the final calysto_scheme/scheme.py.

You will need [Chez Scheme](https://github.com/cisco/ChezScheme) (other Scheme implementations may also work). Chez Scheme is now open source. When you install it, call it `scheme`.

Run `make` in the src directory. This will build calysto_scheme/scheme.py. You can run this file directly from Python:

```
python3 calysto_scheme/scheme.py
```

From there you can also use the DEBUG flag. Very handy with ipython's %debug magic and interactive flag:

```
$ ipython3 -i calysto_scheme/scheme.py
Calysto Scheme, version 1.3.0
----------------------------
Use (exit) to exit
==> (set! DEBUG #t)
==> (sum 1) ;; do something that causes an unhandled exception
[CRASH]
TypeError: 'int' object is not iterable
In [1] %debug
ipdb> 
```
