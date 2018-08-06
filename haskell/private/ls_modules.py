#!/usr/bin/env python

import fnmatch
import itertools
import os
import sys

if len(sys.argv) < 3:
    sys.exit("Usage: %s <DIRECTORY> <HIDDEN_MODS_FILE> <REEXPORTED_MODS_FILE> <RESULT_FILE>" % sys.argv[0])

root = sys.argv[1]
with open(sys.argv[2], "r") as f:
    hidden_modules = [mod.strip() for mod in f.read().split(",")]
with open(sys.argv[3], "r") as f:
    reexported_modules = (
        mod.strip() for mod in f.read().split(",") if mod.strip()
    )

interface_files = (
    os.path.join(path, f)
    for path, dirs, files in os.walk(root)
    for f in fnmatch.filter(files, '*.hi')
)

modules = (
    os.path.splitext(os.path.relpath(f, start=root))[0].replace("/",".")
    for f in interface_files
)

exposed_modules = (
    m
    for m in modules
    if m not in hidden_modules
)

with open(sys.argv[4], "w") as f:
    sys.stdout = f
    print(", ".join(itertools.chain(exposed_modules, reexported_modules)))
