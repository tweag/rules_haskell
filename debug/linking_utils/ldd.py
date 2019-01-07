import subprocess
import os


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
    for k, v in d.iteritems():
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


### IO functions that find elf dependencies

def get_runpath_dirs(elf):
    """Find all runpath entries.

    Returns:
      { path: unmodified string from DT_RUNPATH
      , absolute_path: fully normalized, absolute path to dir }
    """
    origin = os.path.dirname(elf)
    # TODO: way to get info with less execution overhead
    # TODO: cache the results to prevent more than one call per elf binary
    res = subprocess.check_output("""objdump -x {} | grep RUNPATH | sed 's/^ *RUNPATH *//'""".format(elf), shell = True).strip()
    return [{ 'path': path,
              'absolute_path': os.path.normpath(path.replace("$ORIGIN", origin)) }
            for path in res.strip(":").split(":")
            if path != ""]

def get_needed(elf):
    """Returns the list of DT_NEEDED entries for elf"""
    # TODO: way to get info with less execution overhead
    # TODO: cache the results to prevent more than one call per elf binary
    res = subprocess.check_output("""objdump -x {} | grep NEEDED | sed 's/^ *NEEDED *//'""".format(elf), shell = True).strip()
    return res.strip("\n").split("\n")


### Main utility

# cannot find dependency
LDD_MISSING = "MISSING"
# don't know how to search for dependency
LDD_UNKNOWN = "DUNNO"
LDD_ERRORS = [ LDD_MISSING, LDD_UNKNOWN ]

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
        elf_path: path to ELF file

    Returns: a
    """
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
        if search_res == LDD_MISSING:
            return LDD_MISSING
        else:
            # we keep all other fields the same,
            # just item is the one that does the recursion.
            # This is the part that would normally be done by fmap.
            search_res['item'] = ldd(f, search_res['item'])
            return search_res

    rdirs = get_runpath_dirs(elf_path)
    # if there's no runpath dirs we don't know where to search
    if rdirs == []:
        needed = list_to_dict(const(LDD_UNKNOWN), get_needed(elf_path))
    else:
        needed = list_to_dict(
            lambda name: recurse(search(rdirs, name)),
            get_needed(elf_path)
        )

    result = {
        'runpath_dirs': rdirs,
        'needed': needed
    }
    return f(result)


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
    for k, v in d['needed'].iteritems():
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
    for k, v in d['needed'].iteritems():
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
        for name, dep in deps.iteritems():
            res.update(dep['others'])
            res[name] = dep['mine']
        return res

    return {
        'mine': unused,
        'others': combine_unused(prev),
    }
