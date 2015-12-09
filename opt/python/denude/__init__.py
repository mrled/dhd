"""
A Python module for more easily exploring Python objects
"""

from __future__ import print_function
from pdb import set_trace as strace

import pdb
import inspect

"""
I intend to use the following as my main exploration and debugging tools: 

- strace()   # imported above
- type()     # builtin
- len()      # builtin
- members()  # defined here
"""

# TODO: make this return some metadata about what it is
def members(something):
    for m in inspect.getmembers(something):
        print("{} {}".format(m[0], type(m[0])))
