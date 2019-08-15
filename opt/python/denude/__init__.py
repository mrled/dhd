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


# From https://gist.github.com/techtonik/2151727
# Via https://stackoverflow.com/questions/2654113/how-to-get-the-callers-method-name-in-the-called-method#comment69542591_9812105
def caller_name(skip=2):
    """Get a name of a caller in the format module.class.method
    
       `skip` specifies how many levels of stack to skip while getting caller
       name. skip=1 means "who calls me", skip=2 "who calls my caller" etc.
       
       An empty string is returned if skipped levels exceed stack height
    """
    stack = inspect.stack()
    start = 0 + skip
    if len(stack) < start + 1:
      return ''
    parentframe = stack[start][0]    
    
    name = []
    module = inspect.getmodule(parentframe)
    # `modname` can be None when frame is executed directly in console
    # TODO(techtonik): consider using __main__
    if module:
        name.append(module.__name__)
    # detect classname
    if 'self' in parentframe.f_locals:
        # I don't know any way to detect call from the object method
        # XXX: there seems to be no way to detect static method call - it will
        #      be just a function call
        name.append(parentframe.f_locals['self'].__class__.__name__)
    codename = parentframe.f_code.co_name
    if codename != '<module>':  # top level usually
        name.append( codename ) # function or a method
    del parentframe
    return ".".join(name)
