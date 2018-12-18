import os
import ctypes
from bazel_tools.tools.python.runfiles import runfiles
import subprocess

r = runfiles.Create()

path = r.Rlocation('io_tweag_rules_haskell/tests/cc_haskell_import/hs-lib-b-wrapped.so')

foreignlib = ctypes.cdll.LoadLibrary(path)

# ATTN: If you remove this print *statement* hs_init will segfault!
# If you use the python3 print *function*, it will segfault as well!
# TODO: wtf?
print foreignlib

foreignlib.hs_init()
assert(str(foreignlib.add_one_hs(1)) == "2")
