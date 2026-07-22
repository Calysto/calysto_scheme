"""
Regression tests for the type/bounds half of the fast-prim audit: given a
wrong-typed argument, several _FAST_PRIM_SPECS callables (Scheme.py) did a
raw, unchecked operation (attribute chain, len()/index, isinstance assumed
true) instead of the type check their classic dispatch counterpart
(b_proc_XX_d in the generated scheme.py) already does. Two distinct
severities were found by a systematic audit that compared a direct
top-level call against the same call reached through a nested Phase-2/JIT
closure call:

  - Raw exception leaked (same *shape* as the already-fixed car/cdr bug,
    just not yet extended to these): the rest of the car/cdr-combinator
    family (caar, cadr, cdar, cddr, cadar, caddr, cdddr, cadddr), length,
    symbol->string, round.
  - Silent wrong value -- no exception at all, worse than a leaked one,
    since there's no signal anything went wrong: set-car!/set-cdr!
    (actually mutating a foreign, non-pair object via Python's dynamic
    attribute assignment), vector-length, vector->list, list->vector,
    string-length, string->list, list->string, string->symbol, and `=`.

  Separately (not a type-check gap, found incidentally while fixing
  member's improper-list check): the pre-existing _FAST_PRIM_SPECS entry
  for `member` called a native helper that returns a bare boolean, not
  the matching sublist R7RS requires -- wrong even on a successful,
  well-typed call, not just on bad input. `memq`/`memv`/`assq`/`assv`
  were already correct; only `member` had this.
"""
import calysto_scheme.scheme as scheme


def _to_pylist(x):
    """cons has no __eq__ (falls back to Python identity), so two
    separately-built Scheme lists that print identically are never `==`.
    Convert to a plain Python list/tuple for comparison instead."""
    out = []
    while isinstance(x, scheme.cons):
        out.append(x.car)
        x = x.cdr
    if x is not scheme.symbol_emptylist:
        return (tuple(out), x)  # improper list: (elements, final-tail)
    return out


def _nested(call_src, define_src=""):
    src = f"{define_src} (define (_tb) {call_src}) (define (_td) (_tb)) (_td)"
    return scheme.execute_string_rm(src)


def _exception_parts(result):
    assert scheme.exception_q(result), f"expected exception, got {result!r}"
    exc_obj = result.cdr.car
    return str(exc_obj.cdr.car), str(exc_obj.cdr.cdr.car)


def _baseline_parts(call_src, define_src=""):
    src = f"{define_src} {call_src}"
    return _exception_parts(scheme.execute_string_rm(src))


def _assert_matches_baseline(call_src, define_src=""):
    baseline = _baseline_parts(call_src, define_src)
    nested = _exception_parts(_nested(call_src, define_src))
    assert baseline == nested, f"baseline={baseline} nested={nested}"
    assert baseline[0] == "RunTimeError", (
        f"baseline itself isn't a proper RunTimeError ({baseline}) -- "
        "this test's assumption about the reference behavior is wrong"
    )
    return baseline


# ---------------------------------------------------------------------
# Raw-exception-leak class: rest of the car/cdr combinator family
# ---------------------------------------------------------------------

def test_caar_cdar_cadar_cdddr_cddr_cadddr_on_non_pair():
    for name in ("caar", "cdar", "cadar", "cdddr", "cddr", "cadddr"):
        etype, emsg = _assert_matches_baseline(f"({name} (quote a))")
        assert emsg == f"{name} called on non-pair a"


def test_cadr_caddr_on_improper_list():
    etype, emsg = _assert_matches_baseline("(cadr (cons 1 2))")
    assert emsg == "cadr called on incorrect list structure (1 . 2)"
    etype, emsg = _assert_matches_baseline("(caddr (list 1 2))")
    assert "caddr called on incorrect list structure" in emsg


def test_car_cdr_combinators_still_work_on_valid_input():
    assert scheme.execute_string_rm("(caar (list (list 1 2) 3))") == 1
    assert scheme.execute_string_rm("(cadr (list 1 2 3))") == 2
    assert scheme.execute_string_rm("(caddr (list 1 2 3))") == 3
    assert scheme.execute_string_rm("(cadddr (list 1 2 3 4))") == 4


def test_length_on_improper_list():
    _assert_matches_baseline("(length (cons 1 2))")


def test_symbol_to_string_on_non_symbol():
    etype, emsg = _assert_matches_baseline("(symbol->string 1)")
    assert emsg == "symbol->string called on non-symbol item 1"


def test_round_on_non_number():
    etype, emsg = _assert_matches_baseline("(round (quote a))")
    assert emsg == "round requires exactly one number"
    assert scheme.execute_string_rm("(round 2.5)") == 2


# ---------------------------------------------------------------------
# Silent-wrong-value class: the most severe findings -- no error at all
# ---------------------------------------------------------------------

def test_set_car_on_non_pair_raises_instead_of_corrupting_the_object():
    """Previously silently set a bogus `.car` attribute directly onto the
    non-pair object via Python's dynamic attribute assignment, and
    returned void as if it had succeeded."""
    etype, emsg = _assert_matches_baseline("(set-car! (quote a) 5)")
    assert emsg == "set-car! called on non-pair a"


def test_set_cdr_on_non_pair_raises_instead_of_corrupting_the_object():
    etype, emsg = _assert_matches_baseline("(set-cdr! (quote a) 5)")
    assert emsg == "set-cdr! called on non-pair a"


def test_set_car_and_set_cdr_still_work_on_a_real_pair():
    result = scheme.execute_string_rm(
        "(define p (cons 1 2)) (set-car! p 99) (set-cdr! p 88) p"
    )
    assert result.car == 99 and result.cdr == 88


def test_vector_length_on_non_vector():
    etype, emsg = _assert_matches_baseline("(vector-length (quote a))")
    assert emsg == "vector-length called on incorrect vector structure a"


def test_vector_to_list_on_non_vector():
    etype, emsg = _assert_matches_baseline("(vector->list (quote a))")
    assert emsg == "vector->list called on incorrect vector structure a"


def test_list_to_vector_on_non_list():
    etype, emsg = _assert_matches_baseline("(list->vector (quote a))")
    assert emsg == "list->vector called on incorrect list structure a"


def test_string_length_on_non_string():
    etype, emsg = _assert_matches_baseline("(string-length (quote a))")
    assert emsg == "string-length called on non-string argument"


def test_string_to_list_on_non_string():
    etype, emsg = _assert_matches_baseline("(string->list (quote a))")
    assert emsg == "string->list called on non-string item a"


def test_list_to_string_on_non_list():
    etype, emsg = _assert_matches_baseline("(list->string (quote a))")
    assert emsg == "list->string called on incorrect list structure a"


def test_string_to_symbol_on_non_string():
    etype, emsg = _assert_matches_baseline("(string->symbol 1)")
    assert emsg == "string->symbol called on non-string item 1"


def test_numeric_equal_on_non_numeric_argument():
    etype, emsg = _assert_matches_baseline("(= 1 (quote a))")
    assert emsg == "attempt to apply = on non-numeric argument"


def test_numeric_equal_still_works_on_valid_input():
    assert scheme.execute_string_rm("(= 1 1)") is True
    assert scheme.execute_string_rm("(= 1 2)") is False
    # 3+-arg `=` is a case where the fast path (properly variadic, needed
    # to add the all_numeric_q type check anyway) ended up strictly more
    # capable than the classic dispatch, which itself raises a raw
    # "TypeError: numeric_equal() takes 2 positional arguments but 3 were
    # given" for 3 args (Apply(numeric_equal, args_reg) unpacks all of
    # them positionally into a fixed 2-arg function) -- not a baseline
    # match to preserve, just confirming the fast path itself is correct.
    nested = scheme.execute_string_rm(
        "(define (_teq) (= 1 1 1)) (define (_teqd) (_teq)) (_teqd)"
    )
    assert nested is True


# ---------------------------------------------------------------------
# member's independent, more severe bug: wrong VALUE even on success
# ---------------------------------------------------------------------

def test_member_returns_the_matching_sublist_not_a_boolean():
    """Before this fix, the fast-path `member` returned a bare Python
    True/False (via the native member() helper) instead of the matching
    sublist R7RS requires -- wrong on an ordinary, successful,
    well-typed call, not just a bad-input edge case."""
    found = _nested("(member 2 (list 1 2 3))")
    assert not scheme.exception_q(found)
    assert _to_pylist(found) == [2, 3]

    not_found = _nested("(member 9 (list 1 2 3))")
    assert not_found is False


def test_memq_memv_assq_assv_were_already_correct():
    """Confirmed during the audit that these four (unlike member) already
    returned the correct sublist/pair -- included here as a baseline so a
    future change can't silently regress them the way member was."""
    r1 = scheme.execute_string_rm(
        "(memq (quote b) (list (quote a) (quote b) (quote c)))"
    )
    assert [s.name for s in _to_pylist(r1)] == ['b', 'c']
    assert _to_pylist(scheme.execute_string_rm("(memv 2 (list 1 2 3))")) == [2, 3]
    r2 = scheme.execute_string_rm(
        "(assq (quote b) (list (cons (quote a) 1) (cons (quote b) 2)))"
    )
    assert r2.car.name == 'b' and r2.cdr == 2
    r3 = scheme.execute_string_rm("(assv 2 (list (cons 1 10) (cons 2 20)))")
    assert r3.car == 2 and r3.cdr == 20
