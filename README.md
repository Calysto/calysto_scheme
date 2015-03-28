**Calysto Scheme** is a real Scheme programming language, with full support for continuations, including call/cc. It can also use all Python libraries. Also has some extensions that make it more useful (stepper-debugger, choose/fail, stack traces), or make it better integrated with Python.

Because **Calysto Scheme** uses [MetaKernel](https://github.com/Calysto/metakernel/blob/master/README.rst), it has a fully-supported set of "magics"---meta-commands for additional functionality. This includes running Scheme in parallel. See all of the [MetaKernel Magics](https://github.com/Calysto/metakernel/blob/master/metakernel/magics/README.md).

**Calysto Scheme** in use:

* [CS245: Programming Languages](http://jupyter.cs.brynmawr.edu/hub/dblank/public/CS245%20Programming%20Languages/2014/Programming%20Languages,%20Syllabus.ipynb)
* Videos: https://www.youtube.com/watch?v=2w-iO701g_w

You can install Calysto Scheme with Python3:

```
pip3 install --upgrade calysto-scheme
```

or in the system kernel folder with:

```
sudo pip3 install --upgrade calysto-scheme
```

Use it in the console, qtconsole, or notebook with IPython 3:

```
ipython console --kernel calysto_scheme
ipython qtconsole --kernel calysto_scheme
ipython notebook --kernel calysto_scheme
```

Requires:

* ipython-3.0
* Python2 or Python3
* metakernel (installed automatically)

Calysto Scheme supports:

* continuations
* use of all Python libraries
* choose/fail
* produces stack trace (with line numbers), like Python
* test suite

Planned:

* Object-oriented class definitions and instance creation
* complete Scheme functions (one can fall back to Python for now)

Limitations:

* Currently a couple of magnatudes slower than Python
