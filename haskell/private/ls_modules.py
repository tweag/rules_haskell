#!/usr/bin/env python
#
# Create a list of exposed modules (including reexported modules)
# given a directory full of interface files and the content of the
# global package database (to mine the versions of all prebuilt
# dependencies). The exposed modules are filtered using a provided
# list of hidden modules, and augmented with reexport declarations.

from __future__ import unicode_literals

import collections
import fnmatch
import itertools
import os
import re
import sys
import io

if len(sys.argv) != 6:
    sys.exit("Usage: %s <DIRECTORY> <GLOBAL_PKG_DB> <HIDDEN_MODS_FILE> <REEXPORTED_MODS_FILE> <RESULT_FILE>" % sys.argv[0])

root = sys.argv[1]
global_pkg_db_dump = sys.argv[2]
hidden_modules_file = sys.argv[3]
reexported_modules_file = sys.argv[4]
results_file = sys.argv[5]

with io.open(global_pkg_db_dump, "r", encoding='utf8') as f:
    names = [line.split()[1] for line in f if line.startswith("name:")]
    f.seek(0)
    ids = [line.split()[1] for line in f if line.startswith("id:")]

    # A few sanity checks.
    assert len(names) == len(ids)
    if len(names) != len(set(names)):
        duplicates = [
            name for name, count in collections.Counter(names).items()
            if count > 1
        ]
        sys.exit(
            "\n".join([
                "Multiple versions of the following packages installed: ",
                ", ".join(duplicates),
                "\nThis is not currently supported.",
            ])
        )

    pkg_ids_map = dict(zip(names, ids))

with io.open(hidden_modules_file, "r", encoding='utf8') as f:
    hidden_modules = [mod.strip() for mod in f.read().split(",")]

with io.open(reexported_modules_file, "r", encoding='utf8') as f:
    raw_reexported_modules = (
        mod.strip() for mod in f.read().split(",") if mod.strip()
    )
    # Substitute package ids for package names in reexports, because
    # GHC really wants package ids.
    regexp = re.compile("from (%s):" % "|".join(map(re.escape, pkg_ids_map)))
    reexported_modules = (
        regexp.sub(lambda match: "from %s:" % pkg_ids_map[match.group(1)], mod)
        for mod in raw_reexported_modules
    )

interface_files = (
    os.path.join(path, f)
    for path, dirs, files in os.walk(root)
    for f in fnmatch.filter(files, '*.hi')
)

modules = (
    os.path.splitext(os.path.relpath(f, start=root))[0]
        .replace("/",".")
        .replace("\\",".")
    for f in interface_files
)

exposed_modules = (
    m
    for m in modules
    if m not in hidden_modules
)

with io.open(results_file, "w", encoding='utf8') as f:
    f.write(", ".join(itertools.chain(exposed_modules, reexported_modules)))
