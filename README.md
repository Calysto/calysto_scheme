# Calysto Scheme

**Calysto Scheme** is a real Scheme programming language, with full support for continuations, including call/cc. It can also use all Python libraries. Also has some extensions that make it more useful (stepper-debugger, choose/fail, stack traces), or make it better integrated with Python. For more details on using Calysto Scheme, see:

http://nbviewer.jupyter.org/github/Calysto/calysto_scheme/blob/master/notebooks/Reference%20Guide%20for%20Calysto%20Scheme.ipynb

In Jupyter notebooks, because **Calysto Scheme** uses [MetaKernel](https://github.com/Calysto/metakernel/blob/master/README.rst), it has a fully-supported set of "magics"---meta-commands for additional functionality. This includes running Scheme in parallel. See all of the [MetaKernel Magics](https://github.com/Calysto/metakernel/blob/master/metakernel/magics/README.md).

Calysto Scheme is written in Scheme, and then translated into Python (and other backends). The entire functionality lies in a single Python file: https://github.com/Calysto/calysto_scheme/blob/master/calysto_scheme/src/Scheme.py However, you can easily install it (see below).

**Calysto Scheme** in use:

* [CS245: Programming Languages - 2014, Fall](https://jupyter.brynmawr.edu/services/public/dblank/CS245%20Programming%20Languages/2014-Fall/Programming%20Languages,%20Syllabus.ipynb)
* [CS245: Programming Languages - 2016, Fall](https://jupyter.brynmawr.edu/services/public/dblank/CS245%20Programming%20Languages/2016-Fall/Syllabus.ipynb)
* Videos: https://www.youtube.com/watch?v=2w-iO701g_w

## Install

You can install Calysto Scheme with Python3:

```
pip3 install --upgrade calysto-scheme --user
python3 -m calysto_scheme install --user
```

or in the system kernel folder with:

```
sudo pip3 install --upgrade calysto-scheme
sudo python3 -m calysto_scheme install
```

You can also use the --sys-prefix to install into your virtualenv.

Change pip3/python3 to use a different pip or Python. The version of Python used will determine how Calysto Scheme is run.

Use it in the Jupyter console, qtconsole, or notebook:

```
jupyter console --kernel calysto_scheme
jupyter qtconsole --kernel calysto_scheme
jupyter notebook
```

You can also just use the Python program, but it doesn't have a fancy Read-Eval-Print Loop. Just run:

```
python calysto_scheme/scheme.py
```

## Requires

* Python3
* metakernel (installed automatically)

Calysto Scheme can also be un under PyPy for increased performance.

## Features

Calysto Scheme supports:

* continuations
* use of all Python libraries
* choose/fail - built in fail and try again
* produces stack trace (with line numbers), like Python
* test suite

Planned:

* Object-oriented class definitions and instance creation
* complete Scheme functions (one can fall back to Python for now)

Limitations:

* Runs slow on CPython; try PyPy
