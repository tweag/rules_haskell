import six

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
    for k, v in six.iteritems(d):
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

def get_dynamic_fields(elf_path):
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
    def get_dynamic_field(s):
        """return (name, content)"""
        return _field_matcher.match(s).groups()
    return list(map(get_dynamic_field, dyn_section.splitlines(keepends=True)))

def filter_dynamic_fields(df, key):
    return [v for k, v in df if k == key]

def parse_runpath_dirs(elf_path, elf_dynamic_fields):
    """Parse a RUNPATH entry from an elf header bytestring.

    Returns:
      { path: unmodified string from DT_RUNPATH
      , absolute_path: fully normalized, absolute path to dir }
    """
    fields = filter_dynamic_fields(elf_dynamic_fields, b"RUNPATH")
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
    return [n.decode() for n in filter_dynamic_fields(elf_dynamic_fields, b"NEEDED")]


### Main utility

# cannot find dependency
LDD_MISSING = "MISSING"
# don't know how to search for dependency
LDD_UNKNOWN = "DUNNO"
LDD_ERRORS = [ LDD_MISSING, LDD_UNKNOWN ]

def _ldd(elf_cache, f, elf_path):
    """Same as ldd, but elf_cache is a dict needed for memoizing
    which elf files we already read, because the reading operation is quite
    expensive."""

    def search(rdirs, elf_libname):
        """search for elf_libname in rdirs and return either name or missing"""
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
        """Unfold the subtree of ELF dependencies for a search result"""
        if search_res == LDD_MISSING:
            return LDD_MISSING
        else:
            # we keep all other fields in search_res the same,
            # just item is the one that does the recursion.
            # This is the part that would normally be done by fmap.
            search_res['item'] = _ldd(elf_cache, f, search_res['item'])
            return search_res

    dyn_fields = memoized(
        elf_cache, get_dynamic_fields, elf_path
    )
    rdirs = parse_runpath_dirs(elf_path, dyn_fields)
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

def remove_uninteresting_dependencies(d):
    """Filter that removes some uninteresting .sos and everything that points to the nix store. Can be abstracted later."""
    def bad_needed_p(k):
        "predicate for unneeded .sos"
        names = [
            'libc.so.6',
            'ld-linux-x86-64.so.2',
            'libgmp.so.10',
            'libm.so.6',
        ]
        return (k in names)
    def bad_runpath_p(p):
        "predicate for unneeded paths"
        prefixes = [
            "/nix/store/"
        ]
        return any(p.startswith(pref) for pref in prefixes)

    runpaths = []
    for dir in d['runpath_dirs']:
        absp = dir['absolute_path']

        # TODO: put in different test, this is interesting info!
        # non-existing RUNPATHs
        if not os.path.exists(absp):
            print("ATTN path doesnt exist: {}".format(absp))

        if not bad_runpath_p(absp):
            runpaths.append(absp)

    needed = {}
    for k, v in six.iteritems(d['needed']):
        # filter out some uninteresting deps
        if not bad_needed_p(k):
            needed[k] = v['item'] if not v in LDD_ERRORS else v

    return dict_remove_empty({
        'runp': runpaths,
        'need': needed,
    })


def was_runpath_used(d):
    """returns a dict of two fields; `others` contains a flat dict of all .sos with unused runpath entries and a list of them for each .so"""
    used = set()
    given = set(r['absolute_path'] for r in d['runpath_dirs'])
    prev = {}
    for k, v in six.iteritems(d['needed']):
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
        for name, dep in six.iteritems(deps):
            res.update(dep['others'])
            res[name] = dep['mine']
        return res

    return {
        'mine': unused,
        'others': combine_unused(prev),
    }
