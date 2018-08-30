"""
Testing and Coverage code for Calysto Scheme

From top-level calysto_scheme directory, call with:

```
python3 -m nose --with-coverage --cover-package calysto_scheme tests
```

"""

import os
import unittest
import string

from calysto_scheme import scheme

current_directory = os.path.dirname(__file__)
filename = os.path.join(current_directory, "../calysto_scheme/modules/test_all.ss")
test_names = {}

class TestScheme(unittest.TestCase):
    """
    Class to hold the Scheme unit tests.
    """

def get_count(group_name, case_name):
    name = "%s_%s" % (group_name, case_name)
    count = test_names.get(name, -1)
    count += 1
    test_names[name] = count
    return count

def replace(c):
    return c if c in string.ascii_lowercase else "_"

def callback(group_name, case_name, result, traceback, proc_exp, test_exp, result_exp):
    case_name = ''.join(replace(c) for c in case_name.lower())
    group_name = str(group_name)
    count = get_count(group_name, case_name)
    test_name = 'test_%s_%s_%s' % (group_name, case_name, count)
    compare = """
Procedure: %s
         : %s
         : %s
""" % (proc_exp, test_exp, result_exp)
    def test(self):
        self.assertTrue(result, "%s %s %s:\n%s%s" % (group_name, repr(case_name), count, traceback, compare))
    setattr(TestScheme, test_name, test)

## Add dynamic tests:
    
scheme.make_test_callback = callback
retval = scheme.execute_file_rm(filename)
if scheme.exception_q(retval):
    traceback = scheme.get_traceback_string(retval)
    ename, evalue = scheme.get_exception_values(retval)
    print(traceback)
    raise Exception(ename)

if __name__ == "__main__":
    unittest.main()
