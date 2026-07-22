"""
Subprocess helper for tests that depend on process-wide singleton state
(_fast_prim_map, _jit_cache) starting completely fresh -- see
test_primitive_redefinition.py. Executes a single Scheme file in a brand
new interpreter process and prints its outcome as one tab-separated line:

  VALUE\t<repr of the last top-level form's value>
  EXCEPTION\t<exception type>\t<message>

Run as a subprocess (not imported), the same way _phase2_diff_runner.py
is, so each invocation starts from a completely fresh _fast_prim_map /
_jit_cache -- state that is built lazily, once, and cached for the life
of the process, which is exactly the mechanism the bugs under test
exploit (see README-PERFORMANCE.md and test_primitive_redefinition.py).
"""
import sys

import calysto_scheme.scheme as scheme

path = sys.argv[1]
result = scheme.execute_file_rm(path)

if scheme.exception_q(result):
    exc_obj = result.cdr.car
    etype = exc_obj.cdr.car
    emsg = exc_obj.cdr.cdr.car
    print("EXCEPTION\t%s\t%s" % (etype, emsg))
else:
    print("VALUE\t%r" % (result,))
