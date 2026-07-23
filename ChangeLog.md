## Release 2.1.6 (Jul 23, 2026)

	* Added `(use-jit [BOOLEAN])`, a new primitive mirroring
	  `use-stack-trace`: get or set a live `*use-jit*` flag (default
	  `#t`) that gates all three Phase 2/JIT entry points (`apply_proc`,
	  `_apply_direct`, `_jit_call`), letting any code -- not just
	  closures containing `set!` -- be forced onto the always-correct
	  trampoline path, e.g. for debugging or profiling.
	* Backported the `zero?`/`expt`/`memv`/`assv`/`number->string`
	  arity-tightening fix (2.1.3) into `interpreter-cps.ss`, the true
	  CPS source it was missing from; that commit had hand-patched the
	  generated `source-rm.ss` and `scheme.py` directly, which a later
	  full pipeline regeneration would have silently reverted.

## Release 2.1.5 (Jul 23, 2026)

	* Fixed a real JIT inlining bug: `odd?` compiled to a raw
	  `n % 2 != 0`, diverging from the real primitive (`n % 2 == 1`) for
	  any non-integer argument -- e.g. `(odd? 2.5)` returned `#t` from
	  JIT-compiled code and `#f` everywhere else. Found by a new
	  differential fuzzer (`tests/test_jit_fuzz.py`) that generates many
	  random small Scheme programs and compares the trampoline, Phase 2,
	  and JIT execution paths on each one; see `tests/test_jit_odd_float.py`
	  for the pinned regression.
	* Extended the JIT/Phase-2 tag-parity guard tests
	  (`tests/test_jit_tag_parity.py`) to also cover `_phase2_safe_walk`,
	  closing the same class of gap for the walker whose certification
	  errors are the most dangerous (a wrongly-certified proc can commit
	  `apply_proc` to a live Phase-2 attempt and silently re-run side
	  effects on fallback).

## Release 2.1.4 (Jul 23, 2026)

	* Fixed a latent JIT/Phase-2 safety gap: `_apply_direct` (used by
	  map/for-each's fast-prim callbacks and as `_jit_call`'s fallback for
	  a computed-operator call) gated execution with only a closure's own
	  shallow `proc[5]` check, not the full transitive `_is_phase2_safe`
	  certification `apply_proc` requires -- a closure that itself called
	  a `set!`-using helper could start executing and only fail partway
	  through. Confirmed unreachable via any real program today, but
	  closed so a future JIT change can't silently reopen it. See
	  `tests/test_apply_direct_proc5_gap.py`.

## Release 2.1.3 (Jul 23, 2026)

	* Fixed an N-ary comparison crash (`<`, `>`, `<=`, `>=` with 3+ args) and
	  a classic-dispatch/JIT divergence for `=`
	* Fixed JIT-inlined `+` diverging from the classic dispatch on IEEE-754
	  negative zero
	* Tightened arity checks on `zero?`, `expt`, `memv`, `assv`; added radix
	  support (e.g. `(number->string 255 16)`) to `number->string`
	* Fixed stale JIT captures and stale Phase-2 safety certification after
	  a primitive is redefined
	* Fixed two JIT correctness bugs: sibling closures not sharing a frame,
	  and tail-loop non-tail statements only running on the first iteration
	* Closed several JIT/Phase-2 correctness gaps found via a call/cc and
	  choose/require backtracking edge-case audit; added JIT-OVERVIEW.md
	  documenting the JIT design and how it's tested

## Release 2.1.2 (Jul 22, 2026)

	* Fixed JIT/Phase-2 correctness gaps: primitive redefinition could
	  corrupt calls to unrelated closures; fast-path errors leaked raw
	  Python exceptions instead of proper Scheme conditions; audited and
	  fixed arity/type checking across all 83 fast-path primitives
	* Named proc-tuple field indices in cold (compile-once) JIT/Phase-2
	  code paths for clarity (no behavior change)

## Release 2.1.1 (Jul 21, 2026)

	* JIT: self-recursive non-tail calls now call the compiled function
	  directly instead of through an indirection wrapper (~2.3-2.8x faster
	  on naive/tree recursion)
	* Fixed a silent double-execution bug: Phase 2's fallback path could
	  re-run part of a closure's body -- and its side effects -- when it
	  hit an unsupported construct partway through; replaced with an
	  up-front, whole-call-graph safety certification
	* Fixed a pre-existing test-framework infinite loop; disabled stack
	  traces by default (~10-13% faster, ~25-45% less memory)
	* Reduced duplication across the JIT/Phase-2 machinery (shared identity
	  cache, operator-resolution helpers, fast-primitive table)

## Release 2.0.3 (Jul 21, 2026)

	* Fixed a TypeError crash calling `map`/`for-each` with a lambda from
	  JIT-eligible code
	* Added a benchmarking suite (`scripts/benchmark.py`) and cross-version
	  performance comparisons

## Release 2.0.2 (Jul 21, 2026)

	* Fixed a RecursionError crash in deep tail-recursive loops (past
	  ~5,000 iterations) by adding tail-call optimization to the JIT and
	  the direct-eval fast path
	* Added JIT support for closures/higher-order functions and for a
	  parameter used in operator position (e.g.
	  `(define (apply-twice f x) (f (f x)))`)
	* Fixed a `'tuple' object is not callable` crash when a computed
	  operator evaluated to a closure at runtime
	* Fixed `set!` on a function parameter silently writing to a
	  same-named module-level global instead of the parameter

## Release 2.0.1 (Jul 20, 2026)

	* Allowed `try` to be shadowed as a plain identifier again, without
	  reintroducing the earlier silent-swallow bug
	* Added a GitHub Actions test workflow (Python 3.9-3.12)
	* Fixed the `calysto.display.HTML` example in the reference notebook

## Release 2.0.0 (Jul 20, 2026)

	* Major performance overhaul: trampoline optimizations, a direct-eval
	  fast path, and JIT compilation to Python for eligible closures
	  (~4,500x speedup on some benchmarks)
	* Added support for `%plot inline` magic

## Release 1.4.8 (May 16, 2023)

	* Reimplemented dictionaries; dict keys are always strings
	* Revised `dlr_apply` to handle kwargs as a dict
	* Fixed `sort` and `procedure?` primitives; restructured the Makefile
	* Removed `try` without `catch`; removed the `^` primitive
	* Fixed unparse and the `case` macro
	* Fixed the "Unhandled exception" message
	* Build/install cleanup: Docker updates, additional install
	  requirements, added LICENSE.txt

## Release 1.4.7 (Nov 25, 2021)

	* Updated `collections.Iterable` to `collections.abc.Iterable` for
	  newer Python versions
	* Fixed the REPL when running without yasi installed

## Release 1.4.6 (Sep 5, 2018)

	* fixed typo in run-tests that prevented showing incorrect count
	* better debugging
	* show additional info in run-tests verbose mode
	* more annotated info for some tracebacks

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
