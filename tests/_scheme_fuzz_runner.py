"""
Subprocess helper for test_jit_fuzz.py -- the differential-fuzzing
counterpart to _phase2_diff_runner.py. Where that file replays the fixed,
hand-written test_all.ss suite under three execution modes,
this generates a large batch of small random Scheme programs
(_scheme_fuzz_gen.gen_cases) and evaluates each one directly, once per
mode, in a single process (so a batch of hundreds of cases costs one
subprocess launch, not one per case -- state built by earlier cases in
the batch, e.g. _jit_cache/_phase2_safe_cache/_fast_prim_map entries for
their own distinct closures, is inert for later cases since every case
uses fresh, uniquely-numbered function names).

Modes, identical meaning to _phase2_diff_runner.py:
  - "fast":       normal -- Phase 2/JIT enabled, gated by _is_phase2_safe.
  - "slow":       _is_phase2_safe monkeypatched to always return False,
                  forcing every closure call through the register-machine
                  trampoline -- the always-correct baseline.
  - "phase2only": _jit_compile_proc monkeypatched to a no-op, so every
                  Phase-2-eligible closure runs through _eval_direct
                  alone, _JitCompiler never engaging.

Prints one JSON line to stdout: a list of per-case outcomes, in case
order, each one of:
  ["value", <repr(result)>]
  ["scheme-exception", <exception type str>, <message str>]
  ["python-crash", <repr(exception)>]

"python-crash" (an uncaught Python-level exception escaping
execute_string_rm itself, as opposed to a caught-and-wrapped Scheme
exception) is deliberately captured per-case rather than left to crash
the whole batch -- that would both lose every subsequent case's result
and is itself exactly the kind of bug this fuzzer exists to find.
"""
import json
import os
import sys
import warnings

mode = sys.argv[1]
seed = int(sys.argv[2])
count = int(sys.argv[3])
assert mode in ("fast", "slow", "phase2only")

# The JIT deliberately compiles `(not <int-or-other-non-bool>)` to a raw
# Python `<expr> is False` (see _JitCompiler._UNARY) -- semantically
# correct for any object compared against the False singleton, but CPython
# warns on it when <expr> is a literal int. Expected, not a bug; silenced
# so it doesn't look like fuzzer-discovered noise in CI output.
warnings.filterwarnings("ignore", category=SyntaxWarning)

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _scheme_fuzz_gen import gen_cases  # noqa: E402

import calysto_scheme.scheme as scheme  # noqa: E402

if mode == "slow":
    scheme._is_phase2_safe = lambda proc, _visiting=None: False
elif mode == "phase2only":
    scheme._jit_compile_proc = lambda proc: None

results = []
for kind, src in gen_cases(seed, count):
    try:
        result = scheme.execute_string_rm(src)
    except Exception as e:
        results.append(["python-crash", repr(e)])
        continue
    if scheme.exception_q(result):
        exc_obj = result.cdr.car
        etype = exc_obj.cdr.car
        emsg = exc_obj.cdr.cdr.car
        results.append(["scheme-exception", str(etype), str(emsg)])
    else:
        results.append(["value", repr(result)])

print(json.dumps(results))
