
## CalystoScheme (the Jupyter kernel class) pulls in metakernel -> ipykernel
## -> IPython's whole terminal-console stack, which cost every import of
## this package (including ones that only want `calysto_scheme.scheme` to
## run Scheme code, e.g. scripts/benchmark.py and the test suite) a real,
## measured ~200ms on CPython / ~500ms on PyPy for a class most importers
## never touch. __main__.py, the actual kernel entry point, already imports
## CalystoScheme directly from .kernel and doesn't rely on this. Lazy
## module-level __getattr__ (PEP 562) keeps `from calysto_scheme import
## CalystoScheme` working for anyone else relying on it, without paying
## the cost unless that name is actually accessed.
def __getattr__(name):
    if name == "CalystoScheme":
        from .kernel import CalystoScheme
        return CalystoScheme
    raise AttributeError("module %r has no attribute %r" % (__name__, name))
