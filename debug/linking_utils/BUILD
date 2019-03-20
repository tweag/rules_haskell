load(
    ":ldd_test.bzl",
    "ldd_test",
)

py_library(
    name = "linking_utils",
    srcs = ["ldd.py"],
    visibility = ["//visibility:public"],
)

# test the ldd debug library on the output of `//tests/binary-indirect-cbits`
ldd_test(
    name = "test-ldd",
    current_workspace = None,
    elf_binary = "//tests/binary-indirect-cbits",
    script = r'''
import sys

def contains_error(error):
    """check whether any of the dependencies contains `error`,
    where error is something from `LDD_ERRORS`.
    Returns {} if there's no error.
    """
    def f(d):
        return { k: v for k, v in d['needed'].items()
          if (v == error
             or (v not in LDD_ERRORS
                and dict_remove_empty(v['item']) != {})) }
    return f

# output should have some runpaths
assert \
    ldd(identity, sys.argv[1])['runpath_dirs']\
    > 0

# some of the dependencies are implicit and not in NEEDED flags
assert ldd(contains_error(LDD_UNKNOWN), sys.argv[1])

import pprint
# none of the dependencies must be missing
res = ldd(contains_error(LDD_MISSING), sys.argv[1])
if res != {}:
  print("These dependencies are missing:")
  pprint.pprint(res)
  exit(1)
''',
    # it only works on linux
    tags = ["dont_test_on_darwin"],
)
