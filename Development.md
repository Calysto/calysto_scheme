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

Run `make` in the src directory. This will build calysto_scheme/scheme.py.
