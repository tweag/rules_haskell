import subprocess
import os
import sys
import re


### helper functions

def list_to_dict(f, l):
    """dict with elements of list as keys & as values transformed by f"""
    d = {}
    for el in l:
        d[el] = f(el)
    return d

def dict_remove_empty(d):
    """remove keys that have [] or {} or as values"""
    new = {}
    for k, v in d.items():
        if not (v == [] or v == {}):
             new[k] = v
    return new

def identity(x):
    """identity function"""
    return x

def const(x):
    """(curried) constant function"""
    def f(y):
        return x
    return f

def memoized(cache, f, arg):
    """Memoizes a call to `f` with `arg` in the dict `cache`.
    Modifies the cache dict in place."""
    res = cache.get(arg)
    if arg in cache:
        return cache[arg]
    else:
        res = f(arg)
        cache[arg] = res
        return res

### IO functions that find elf dependencies

_field_matcher = re.compile(b"  ([A-Z0-9_]+) +(.*)$")

def read_dynamic_fields(elf_path):
    """Read the dynamic header fields from an elf binary

    Args:
      elf_path: path to the elf binary (either absolute or relative to pwd)

    Returns:
      a list [(field_key, field_value)] where field_keys could appear multiple
      times (for example there's usually more than one NEEDED field).
    """
    res = subprocess.check_output([
        # force locale to C for stable output
        "env", "LC_ALL=C",
        "objdump",
        # specifying the section brings execution time down from 150ms to 10ms
        "--section=.dynamic",
        "--all-headers",
        elf_path
    ])
    to_end = res.split(b"Dynamic Section:\n")[1]
    # to first empty line
    dyn_section = to_end[: 1 + to_end.find(b"\n\n")]
    def read_dynamic_field(s):
        """return (field_key, field_value)"""
        return _field_matcher.match(s).groups()
    return list(map(read_dynamic_field, dyn_section.splitlines(True)))

def __query_dynamic_fields(df, key):
    """takes a list of dynamic field tuples (key and value),
    where keys can appear multiple times, and returns a list of all
    values with the given key (in stable order)."""
    return [v for k, v in df if k == key]

def parse_runpath_dirs(elf_path, elf_dynamic_fields):
    """Parse a RUNPATH entry from an elf header bytestring.

    Returns:
      { path: unmodified string from DT_RUNPATH
      , absolute_path: fully normalized, absolute path to dir }
    """
    fields = __query_dynamic_fields(elf_dynamic_fields, b"RUNPATH")
    if fields == []:
        return []
    assert len(fields) == 1
    val = fields[0]
    origin = os.path.dirname(elf_path)
    return [{ 'path': path,
              'absolute_path': os.path.abspath(path.replace("$ORIGIN", origin)) }
            for path in val.decode().strip(":").split(":")
            if path != ""]

def parse_needed(elf_dynamic_fields):
    """Returns the list of DT_NEEDED entries for elf"""
    return [n.decode() for n in __query_dynamic_fields(elf_dynamic_fields, b"NEEDED")]


### Main utility

# cannot find dependency
LDD_MISSING = "MISSING"
# don't know how to search for dependency
LDD_UNKNOWN = "DUNNO"
# list of all errors for easy branching
LDD_ERRORS = [ LDD_MISSING, LDD_UNKNOWN ]

def _ldd(elf_cache, f, elf_path):
    """Same as `ldd` (below), except for an additional `elf_cache` argument,
    which is a dict needed for memoizing elf files that were already read.
    This is done because the elf reading operation is quite expensive
    and many files are referenced multiple times (e.g. glib.so)."""

    def search(rdirs, elf_libname):
        """search for elf_libname in runfile dirs
        and return either the name or missing"""
        res = LDD_MISSING
        for rdir in rdirs:
            potential_path = os.path.join(rdir['absolute_path'], elf_libname)
            if os.path.exists(potential_path):
                res = {
                    'item': potential_path,
                    'found_in': rdir,
                }
                break
        return res

    def recurse(search_res):
        """Unfold the subtree of ELF dependencies for a `search` result"""
        if search_res == LDD_MISSING:
            return LDD_MISSING
        else:
            # we keep all other fields in search_res the same,
            # just item is the one that does the recursion.
            # This is the part that would normally be done by fmap.
            search_res['item'] = _ldd(elf_cache, f, search_res['item'])
            return search_res

    # (GNU) ld.so resolves any symlinks before searching for dependencies
    elf_realpath = os.path.realpath(elf_path)

    # memoized uses the cache to not repeat the I/O action
    # for the same elf files (same path)
    dyn_fields = memoized(
        elf_cache, read_dynamic_fields, elf_realpath
    )
    rdirs = parse_runpath_dirs(elf_realpath, dyn_fields)
    all_needed = parse_needed(dyn_fields)

    # if there's no runpath dirs we don't know where to search
    if rdirs == []:
        needed = list_to_dict(const(LDD_UNKNOWN), all_needed)
    else:
        needed = list_to_dict(
            lambda name: recurse(search(rdirs, name)),
            all_needed
        )

    result = {
        'runpath_dirs': rdirs,
        'needed': needed
    }
    # Here, f is applied to the result of the previous level of recursion
    return f(result)


def ldd(f, elf_path):
    """follows DT_NEEDED ELF headers for elf by searching the through DT_RUNPATH.

    DependencyInfo :
    { needed : dict(string, union(
        LDD_MISSING, LDD_UNKNOWN,
        {
            # the needed dependency
            item : a,
            # where the dependency was found in
            found_in : RunpathDir
        }))
    # all runpath directories that were searched
    , runpath_dirs : [ RunpathDir ] }

    Args:
        f: DependencyInfo -> a
        modifies the results of each level
        elf_path: path to ELF file, either absolute or relative to current working dir

    Returns: a
    """
    elf_cache = {}
    return _ldd(elf_cache, f, elf_path)


### Functions to pass to ldd

# Only use the current layer

def remove_matching_needed(d, re_matcher_absolute_path=None, re_matcher_path=None):
    """Destructively removes needed values from d['needed']
    if they match the given regex matcher.
    Doesn't remove LDD_ERRORS."""
    def pred(v):
        """return true if match"""
        if v in LDD_ERRORS:
            return False
        found_in = v['found_in']
        abs_match = re_matcher_absolute_path.match(found_in['absolute_path']) \
                    if re_matcher_absolute_path else False
        match = re_matcher_path.match(found_in['path']) \
                    if re_matcher_path else False
        if abs_match or match:
            return True
    d['needed'] = {
        k: v for k, v in d['needed'].items()
        if not pred(v)
    }

def remove_matching_runpaths(d, re_matcher):
    """Destructively removes runpaths from d['runpath_dirs']
    if they match the given regex matcher."""
    d['runpath_dirs'] = [
        runp for runp in d['runpath_dirs']
        if not re_matcher.match(runp['absolute_path'])
    ]
    return d

def non_existing_runpaths(d):
    """Return a list of runpaths_dirs that do not exist in the file system."""
    return [
        runp for runp in d['runpath_dirs']
        if not os.path.exists(runp['absolute_path'])
    ]

def unused_runpaths(d):
    """Return a list of runpath_dirs that were not used to find NEEDED dependencies."""
    used = set()
    for k, v in d['needed'].items():
        if not v in LDD_ERRORS:
            used.add(v['found_in']['absolute_path'])
    return [
        u for u in d['runpath_dirs']
        if u['absolute_path'] not in used
    ]

# Also use the results of sub-layers

def collect_unused_runpaths(d):
    """This is like `unused_runpaths`, but it creates a deduplicated list of all unused runpaths
    for its dependencies instead of just returning them for the current layer.

    Returns:
      a dict of two fields;
      `mine` contains the unused dependencies of the current binary under scrutiny
      `others` contains a flat dict of all .sos with unused runpath entries and a list of them for each .so
    """
    used = set()
    given = set(r['absolute_path'] for r in d['runpath_dirs'])
    prev = {}
    # TODO: use `unused_runpaths` here
    for k, v in d['needed'].items():
        if not v in LDD_ERRORS:
            used.add(v['found_in']['absolute_path'])
            prev[k] = v['item']
    unused = [
        u for u in given.difference(used)
        # leave out nix storepaths
        if not u.startswith("/nix/store")
    ]

    # Each layer doesn't know about their own name
    # So we return a list of unused for this layer ('mine')
    # and a dict of all previeous layers combined (name to list)
    def combine_unused(deps):
        res = {}
        for name, dep in deps.items():
            res.update(dep['others'])
            res[name] = dep['mine']
        return res

    return {
        'mine': unused,
        'others': combine_unused(prev),
    }
