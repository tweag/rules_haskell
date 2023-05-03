import os
import ctypes
from python.runfiles import runfiles
import subprocess

r = runfiles.Create()

path = r.Rlocation('rules_haskell/tests/cc_haskell_import/hs-lib-b-wrapped.so')

foreignlib = ctypes.cdll.LoadLibrary(path)
foreignlib.hs_init.restype = ctypes.c_int
foreignlib.hs_init.argtypes = [ctypes.c_int, ctypes.POINTER(ctypes.POINTER(ctypes.c_char))]

foreignlib.hs_init(0, None)
assert(str(foreignlib.add_one_hs(ctypes.c_int(1))) == "2")
