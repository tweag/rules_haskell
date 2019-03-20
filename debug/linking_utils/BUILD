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
import subprocess

def contains_error(error):
    """check whether any of the dependencies contains `error`,
    where error is something from `LDD_ERRORS`.
    """
    def f(d):
        is_any_err = False
        example = None
        for k, n in d['needed'].items():
            if is_any_err:
              break
            if n is error:
                is_any_err = True
                example = k
                break
            elif n in LDD_ERRORS:
                pass
            else:
                is_any_err = n['item']['is_error']
                example = n['item']['example']
        return { 'is_error': is_any_err, 'example': example }
    return f

# output should have some runpaths
assert \
    ldd(identity, sys.argv[1])['runpath_dirs']\
    > 0

# some of the dependencies are implicit and not in NEEDED flags
assert ldd(contains_error(LDD_UNKNOWN), sys.argv[1])

# none of the dependencies must be missing
subprocess.check_call(["ldd", sys.argv[1]])
res = ldd(contains_error(LDD_MISSING), sys.argv[1])
if res['is_error']:
  print("example of missing dependency: {}".format(res['example']))
  exit(1)
''',
    # it only works on linux
    tags = ["dont_test_on_darwin"],
)
