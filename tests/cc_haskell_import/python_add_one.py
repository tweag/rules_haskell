import os
import ctypes
from bazel_tools.tools.python.runfiles import runfiles
import subprocess

r = runfiles.Create()

# print("runfiles manifest: {}".format(r._strategy._path))
# for k, f in r._strategy._runfiles.iteritems():
#     # print("{}    -> {}".format(k, f))
#     print(f)

path = r.Rlocation('io_tweag_rules_haskell/tests/cc_haskell_import/libadd_one.so')

subprocess.call("find $RUNFILES_DIR", shell=True)

# subprocess.call(["ldd", path])
# subprocess.call("objdump -x {} | grep -i runpath".format(path), shell=True)
# print("path: {}".format(path))

foreignlib = ctypes.cdll.LoadLibrary(path)

# ATTN: If you remove this print(), hs_init will segfault!
# TODO: why?
print(foreignlib)

foreignlib.hs_init()
print('one plus one equals: ' + str(foreignlib.add_one_hs(1)))
print('success!')
