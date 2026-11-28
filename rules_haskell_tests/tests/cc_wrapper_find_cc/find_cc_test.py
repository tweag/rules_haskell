"""Tests for how cc_wrapper locates the C compiler."""

import importlib.machinery
import importlib.util
import os
import sys
import tempfile
import unittest

from python.runfiles import runfiles

TEMPLATE = sys.argv.pop(1)


def load_cc_wrapper(platform, cc):
    os.environ["CC_WRAPPER_PLATFORM"] = platform
    os.environ["CC_WRAPPER_CC_PATH"] = cc
    path = runfiles.Create().Rlocation(TEMPLATE)
    loader = importlib.machinery.SourceFileLoader("cc_wrapper_" + platform, path)
    spec = importlib.util.spec_from_loader(loader.name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


class FindCcTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(self.rmtree)
        self.cc = os.path.join(self.dir, "gcc")

    def rmtree(self):
        for name in os.listdir(self.dir):
            os.remove(os.path.join(self.dir, name))
        os.rmdir(self.dir)

    def touch(self, path):
        with open(path, "w"):
            pass

    def test_windows_finds_compiler_without_exe_suffix(self):
        self.touch(self.cc + ".exe")
        self.assertEqual(load_cc_wrapper("windows", self.cc).find_cc(), self.cc + ".exe")

    def test_windows_keeps_exe_suffix(self):
        self.touch(self.cc + ".exe")
        self.assertEqual(
            load_cc_wrapper("windows", self.cc + ".exe").find_cc(),
            self.cc + ".exe",
        )

    def test_existing_path_is_kept(self):
        self.touch(self.cc)
        self.touch(self.cc + ".exe")
        self.assertEqual(load_cc_wrapper("windows", self.cc).find_cc(), self.cc)
        self.assertEqual(load_cc_wrapper("linux", self.cc).find_cc(), self.cc)


if __name__ == "__main__":
    unittest.main()
