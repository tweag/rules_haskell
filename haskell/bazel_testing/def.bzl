load("//haskell:def.bzl", "haskell_test")

def haskell_bazel_test(rule_files = None, **kwargs):
    """haskell_bazel_test is a wrapper for haskell_test that simplifies the use of
    //haskell/bazel_testing. Tests may be written
    that don't explicitly depend on bazel_testing or rules_haskell files.
    """

    if not rule_files:
        rule_files = ["@rules_haskell//:all_files"]

    # Add dependency on bazel_testing library.
    kwargs.setdefault("deps", [])

    bazel_testing_library = "@rules_haskell//haskell/bazel_testing"
    if bazel_testing_library not in kwargs["deps"]:
        kwargs["deps"] += [bazel_testing_library]

    # Add data dependency on rules_haskell files. bazel_testing will copy or link
    # these files in an external repo.
    kwargs.setdefault("data", [])
    kwargs["data"] += rule_files

    # Add paths to rules_go files to arguments. bazel_testing will copy or link
    # these files.
    kwargs.setdefault("args", [])
    kwargs["args"] = (["-begin_files"] +
                      ["$(locations {})".format(t) for t in rule_files] +
                      ["-end_files"] +
                      kwargs["args"])

    # Set rundir to the workspace root directory to ensure relative paths
    # are interpreted correctly.
    kwargs.setdefault("rundir", ".")

    # Set tags.
    # local: don't run in sandbox or on remote executor.
    # exclusive: run one test at a time, since they share a Bazel
    #   output directory. If we don't do this, tests must extract the bazel
    #   installation and start with a fresh cache every time, making them
    #   much slower.
    kwargs.setdefault("tags", [])
    if "local" not in kwargs["tags"]:
        kwargs["tags"] += ["local"]
    if "exclusive" not in kwargs["tags"]:
        kwargs["tags"] += ["exclusive"]

    haskell_test(**kwargs)
