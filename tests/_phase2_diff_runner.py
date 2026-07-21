"""
Helper subprocess for test_phase2_safety.py.

Runs the full test_all.ss suite once, in a single mode:
  - "fast":       normal -- Phase 2/JIT enabled, gated by _is_phase2_safe.
  - "slow":       _is_phase2_safe monkeypatched to always return False,
                  forcing every closure call through the register-machine
                  trampoline -- the always-correct baseline.
  - "phase2only": _jit_compile_proc monkeypatched to a no-op (never
                  compiles, cache stays empty), so every Phase-2-eligible
                  closure runs through _eval_direct alone, with _JitCompiler
                  never engaging at all -- isolates _eval_direct's own
                  behavior from _JitCompiler's.
Prints one JSON line per assertion to stdout: [group_name, case_name, passed].

Run as a subprocess (not imported) so each mode starts from a completely
fresh interpreter process -- no shared _jit_cache / _phase2_safe_cache
state between the runs being compared.
"""
import json
import os
import sys

mode = sys.argv[1]
assert mode in ("fast", "slow", "phase2only")

import calysto_scheme.scheme as scheme

if mode == "slow":
    scheme._is_phase2_safe = lambda proc, _visiting=None: False
elif mode == "phase2only":
    scheme._jit_compile_proc = lambda proc: None

results = []


def callback(group_name, case_name, result, traceback, proc_exp, test_exp, result_exp):
    results.append([str(group_name), str(case_name), bool(result)])


scheme.make_test_callback = callback
here = os.path.dirname(os.path.abspath(__file__))
filename = os.path.join(here, "..", "calysto_scheme", "modules", "test_all.ss")
retval = scheme.execute_file_rm(filename)

if scheme.exception_q(retval):
    print(json.dumps({"exception": scheme.get_traceback_string(retval)}))
    sys.exit(1)

print(json.dumps(results))
