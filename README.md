# Calysto Scheme

[![codecov](https://codecov.io/gh/Calysto/calysto_scheme/branch/master/graph/badge.svg)](https://codecov.io/gh/Calysto/calysto_scheme) [![CircleCI](https://circleci.com/gh/Calysto/calysto_scheme.svg?style=svg)](https://circleci.com/gh/Calysto/calysto_scheme)

You can try Calysto Scheme without installing anything by clicking on the following button:

[![Binder](https://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/Calysto/calysto_scheme/master?filepath=notebooks%2FReference%20Guide%20for%20Calysto%20Scheme.ipynb)

**Calysto Scheme** is a real Scheme programming language, with full support for continuations, including call/cc. It can also use all Python libraries. Also has some extensions that make it more useful (stepper-debugger, choose/fail, stack traces), or make it better integrated with Python. For more details on using Calysto Scheme, see:

http://nbviewer.jupyter.org/github/Calysto/calysto_scheme/blob/master/notebooks/Reference%20Guide%20for%20Calysto%20Scheme.ipynb

In Jupyter notebooks, because **Calysto Scheme** uses [MetaKernel](https://github.com/Calysto/metakernel/blob/master/README.rst), it has a fully-supported set of "magics"---meta-commands for additional functionality. This includes running Scheme in parallel. See all of the [MetaKernel Magics](https://github.com/Calysto/metakernel/blob/master/metakernel/magics/README.md).

Calysto Scheme is written in Scheme, and then translated into Python (and other backends). The entire functionality lies in a single Python file: https://github.com/Calysto/calysto_scheme/blob/master/calysto_scheme/scheme.py However, you can easily install it (see below).

**Calysto Scheme** in use:

* [CS245: Programming Languages - 2014, Fall](https://jupyter.brynmawr.edu/services/public/dblank/CS245%20Programming%20Languages/2014-Fall/Programming%20Languages,%20Syllabus.ipynb)
* [CS245: Programming Languages - 2016, Fall](https://jupyter.brynmawr.edu/services/public/dblank/CS245%20Programming%20Languages/2016-Fall/Syllabus.ipynb)
* Videos: https://www.youtube.com/watch?v=2w-iO701g_w

## Parallel Processing

To use Calysto Scheme in parallel, do the following:

1. Make sure that the Python module `ipyparallel` is installed. In the shell, type:

```
pip install ipyparallel
```

2. To enable the extension in the notebook, in the shell, type:

```
ipcluster nbextension enable
```

3. To start up a cluster, with 10 nodes, on a local IP address, in the shell, type:

```
ipcluster start --n=10 --ip=192.168.1.108
```

4. Initialize the code to use the 10 nodes, inside the notebook from a host kernel (can be any metakernel kernel), type:

```
%parallel calysto_scheme CalystoScheme
```

5. Run code in parallel, inside the notebook, type:

Execute a single line, in parallel:

```
%px (+ 1 1)
```

Or execute the entire cell, in parallel:

```
%%px
(* cluster_rank cluster_rank)
```

Results come back in a Scheme vector, in cluster_rank order. Therefore, the above would produce the result:

```scheme
#10(0 1 4 9 16 25 36 49 64 81)
```
You can get the results back in the host Scheme by accessing the variable `_` (single underscore).

Notice that you can use the variable `cluster_rank` to partition parts of a problem so that each node is working on something different.

In the examples above, use `-e` to evaluate the code in the host Scheme as well. Note that `cluster_rank` is not defined on the host machine, and that this assumes the host kernel is the same as the parallel machines.

A full notebook example can be found here: [Mandelbrot.ipynb](https://github.com/Calysto/metakernel/blob/master/examples/Mandelbrot.ipynb)

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
