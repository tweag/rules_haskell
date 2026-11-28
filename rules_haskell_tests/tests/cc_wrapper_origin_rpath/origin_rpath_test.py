"""Tests for the rpath rewriting of cc_wrapper on Linux."""

import importlib.machinery
import importlib.util
import os
import sys
import tempfile
import unittest

from python.runfiles import runfiles

TEMPLATE = sys.argv.pop(1)


def load_cc_wrapper(platform):
    os.environ["CC_WRAPPER_PLATFORM"] = platform
    path = runfiles.Create().Rlocation(TEMPLATE)
    loader = importlib.machinery.SourceFileLoader("cc_wrapper_" + platform, path)
    spec = importlib.util.spec_from_loader(loader.name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


class OriginRpathTest(unittest.TestCase):
    def setUp(self):
        self.execroot = os.path.realpath(tempfile.mkdtemp())
        self.addCleanup(os.rmdir, self.execroot)
        self.cwd = os.getcwd()
        os.chdir(self.execroot)
        self.addCleanup(os.chdir, self.cwd)
        os.environ.pop("RULES_HASKELL_EXEC_ROOT", None)
        self.linux = load_cc_wrapper("linux")

    def test_execroot_rpath_is_relative_to_origin(self):
        rpath = os.path.join(self.execroot, "external/toolchain/lib")
        self.assertEqual(
            self.linux.origin_rpath(rpath, "bazel-out/k8-fastbuild/bin/pkg/libfoo.so"),
            "$ORIGIN/../../../../external/toolchain/lib",
        )

    def test_execroot_from_environment(self):
        os.environ["RULES_HASKELL_EXEC_ROOT"] = self.execroot
        self.addCleanup(os.environ.pop, "RULES_HASKELL_EXEC_ROOT")
        rpath = os.path.join(self.execroot, "bazel-out/k8-fastbuild/bin/lib")
        self.assertEqual(
            self.linux.origin_rpath(rpath, "bazel-out/k8-fastbuild/bin/pkg/libfoo.so"),
            "$ORIGIN/../lib",
        )

    def test_rpath_outside_execroot_is_kept(self):
        self.assertEqual(
            self.linux.origin_rpath("/usr/lib", "bazel-out/k8-fastbuild/bin/libfoo.so"),
            "/usr/lib",
        )

    def test_sibling_of_execroot_is_kept(self):
        rpath = self.execroot + "-other/lib"
        self.assertEqual(
            self.linux.origin_rpath(rpath, "bazel-out/k8-fastbuild/bin/libfoo.so"),
            rpath,
        )

    def test_relative_rpath_is_kept(self):
        self.assertEqual(
            self.linux.origin_rpath("$ORIGIN/../lib", "bazel-out/k8-fastbuild/bin/libfoo.so"),
            "$ORIGIN/../lib",
        )

    def test_temporary_output_is_kept(self):
        rpath = os.path.join(self.execroot, "external/toolchain/lib")
        output = os.path.join(tempfile.gettempdir(), "ghc123/libfoo.so")
        self.assertEqual(self.linux.origin_rpath(rpath, output), rpath)

    def test_other_platforms_are_kept(self):
        rpath = os.path.join(self.execroot, "external/toolchain/lib")
        darwin = load_cc_wrapper("darwin")
        self.assertEqual(
            darwin.origin_rpath(rpath, "bazel-out/k8-fastbuild/bin/libfoo.dylib"),
            rpath,
        )


if __name__ == "__main__":
    unittest.main()
