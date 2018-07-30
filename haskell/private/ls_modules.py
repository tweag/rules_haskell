#!/usr/bin/env python

import fnmatch
import os
import sys

if len(sys.argv) < 3:
    sys.exit("Usage: %s <DIRECTORY> <RESULT> [HIDDEN_MODULE]..." % sys.argv[0])

root = sys.argv[1]
sys.stdout = open(sys.argv[2], "w+")

interface_files = (
    os.path.join(path, f)
    for path, dirs, files in os.walk(root)
    for f in fnmatch.filter(files, '*.hi')
)

modules = (
    os.path.splitext(os.path.relpath(f, start=root))[0].replace("/",".")
    for f in interface_files
)

hidden_modules = set(sys.argv[3:])

exposed_modules = (
    m
    for m in modules
    if m not in hidden_modules
)

print(" ".join(exposed_modules))
