## Release 1.4.5 (Sep 5, 2018)

	* raise now creates a real exception
	* New primitive (get-exception-message EXCEPTION) to get message from an exception
	* use (raise "message") or (raise (list "Exception" "message"))
		* Exception can be one of: "AssertionError", "Exception", "KeyboardInterrupt", "MacroError", "ParseError", "ReadError", "RunTimeError", "ScanError", or "UnhandledException"
	* (raise ...) now has proper tracebacks

## Release 1.4.4 (Sep 2, 2018)

	* Renamed setitem, hasitem, and getitem
		* setitem -> set-item!
		* hasitem -> has-item?
		* getitem -> get-item
	* Added:
		* set-attr!
		* has-attr?
		* get-attr
	* fixed bug in scheme-host's sort

## Release 1.4.3 (Sep 1, 2018)

	* added requirement yasi to do proper indentation in console
	* yasi also provides signal for do_is_complete without full-on parsing
	* yasi can be customized via ~/.yasirc.json
		* see: https://github.com/nkmathew/yasi-sexp-indenter#customization
	* new scripts: calysto-scheme, calysto-scheme-debug
	* remove "var = undefined" useless statements

## Release 1.4.2 (Aug 31, 2018)

	* new (range) tests
	* fixed (define 1 2)
	* fixed range()
	* speed up on Apply on zero args
	* new (sort) tests
	* fixed (sort)
	* speedups: inline true_q, null_q; re-wrote length_at_least
	* missing special forms in completion: run-tests, define-tests, assert

## Release 1.4.1 (Aug 30, 2018)

	* fixed `stdin to "stdin" differences
	* speed up: car(item) -> item.car
	* speed up: tagged functions, combine length and list?

## Release 1.4.0 (Aug 29, 2018)

	* Moved implementation data structures to datastructures.ss
	* removed filenames from info, use index instead
	* fixed a bug in StringIO strings
	* Optimizations on strings
	* Show verbose output when selecting filtered assertions in run-tests
	* added new string-join
	* Show exception on do_execute_file()
	* Add data_files for kernel.json install
	* Added ~ home expansion in cd (python)
	* assert now raises AssertionError
	* Allow test group names to be strings; allow partial matching
	* New (host-environment)
	* Added (hasitem)
	* check for duplicate unit test group
	* new (clear-unit-tests)
	* New define-syntax via function
	* Added (get-completions), (macros); removed macros from dir
	* memv should use eqv_q
	* Cleanup of exit/quit: tell users to ^D
	* Use correct help_obj in jupyter

## Release 1.3.0 (Aug 15, 2018)

	* Refactor internal defines
	* Fixed bug in random(int) in Python host
	* Added positive_q to Python host
	* Random prim for Python host
	* Better eq? for ints and floats
	* Better repr for Box
	* Changed name to truncate_to_integer
	* int() truncates in Python host
	* Added box functions to Python host
	* Define sort as a prim in Python
	* Fixed sort on Scheme host
	* Redefined symbol? in Python
	* In debugger, print rather than return inspector expr

## Release 1.2.2 (May 5, 2018)

	* Allow named parameters to DLR functions
	* A better do_function_direct
	* Restore error when args don't match
	* Allow use of λ for lambda
	* Added expt - raise to a power
	* Document latex vars and getitem/setitem

## Release 1.2.0 (Aug 27, 2016)

	* latex-style unicode variables
	* complex numbers are numbers

## Release 1.1.9 (Aug 14, 2016)

	* setitem fixed
	* setitem can set properties

## Release 1.1.7: (Aug 11, 2016)

	* proper errors
	* version number in banner

## Release 1.1.6 (Aug 10, 2016)

	* set status on error
	* Allow (dict) with no args
	* (getitem ...) can get attr now; updated docs

## Release 1.1.5 (Aug 4, 2016)

	* Fixed error for Python3
	* Add info on Scheme to Ref Guide
	* Ref Guide notebook updates

## Release 1.1.1 (Aug 3, 2016)

	* fixes for Python2
	* fixed python-eval/exec, added to Ref Guide
	* WIP: Added notebook from class notes
	* Updated a couple of scheme doc-strings
	* Fixed unbound module error message
	* fixed load-as

## Release 1.0.10 (Aug 2, 2016)

	* scheme modules in search path
	* Added SCHEMEPATH

## Release 1.0.8 (Aug 1, 2016)

	* added python-eval, python-exec
	* added ability to run magics in console

## Release 1.0.7 (Jul 29, 2016)

	* add vector-length
	* import CalystoScheme at top

## Release 1.0.6 (Jul 16, 2016)

	* Updated install
	* Fix error in (eqv? long long)
	* A docker image for calysto_scheme; thanks to @joearasin
	* Passing tests for basic defaults and named arguments
	* Allow default values -- though ignored

## Release 1.0.0 (Nov 7, 2015)

	* Use __version__; handle load errors from command line; WIP handle named args/defaults
	* WIP: params and args; renamed = to numeric_equal
	* Allow associations to be used with (dict ...)
	* Moved source from Calico/lanagugaes to its new home here
	* Revise packaging; kernel protection when displaying results
	* Rewrote __len__ to not be recursive
	* Python3 removed operator.add
	* Allow map/for-each to work on generators
	* Better version of the calysto logo
	* Merge pull request #1 from rgbkrk/patch-1
	* Adjust for moved repository
	* Moved from calysto/calysto/language/scheme
	* Initial commit on github

For earlier versions (2015-2011), please see:

https://bitbucket.org/ipre/calico/src/master/languages/Scheme/Scheme/

Earlier versions (back to 2008) lost in cvs at cs.brynmawr.edu.
