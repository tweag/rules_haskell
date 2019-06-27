import os
import ctypes
from bazel_tools.tools.python.runfiles import runfiles
import subprocess

r = runfiles.Create()

path = r.Rlocation('io_tweag_rules_haskell/tests/cc_haskell_import/hs-lib-b-wrapped.so')

foreignlib = ctypes.cdll.LoadLibrary(path)

foreignlib.hs_init()
assert(str(foreignlib.add_one_hs(1)) == "2")
