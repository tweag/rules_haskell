# Debugging linking errors

The usual utilities, like `nm`, `objdump`, and of course `ldd` (see
[here](https://linux-audit.com/elf-binaries-on-linux-understanding-and-analysis/#tools-for-binary-analysis)
for a good overview of existing tools) go a long way. Yet, when
debugging non-trivial runtime linker failures one would oftentimes
like to filter outputs programmatically, with more advanced query
logic than just simple `grep` and `sed` expressions.

This library provides a small set of utility subroutines. These can
help debug complicated linker errors.

The main function is `ldd(f, elf_path)`. It is in the same spirit
as `ldd(1)`, but instead of a flat list of resolved libraries, it
returns a tree of structured information.

When we use the term `ldd` in the following document, it refers
to the `ldd` function exported from [./ldd.py](./ldd.py).

To query that tree, you pass it a function `f`, which is applied to
each dependency recursively (transforming the tree from the bottom
up).

The following functions are exported alongside the `ldd` function.
They can be passed to `ldd` and used as building blocks for insightful
queries:

- `identity`: don’t transform, output everything
- `remove_matching_needed`: remove needed entries that match a regex
- `remove_matching_runpaths`: remove runpaths that match a regex
- `non_existing_runpaths`: return a list of runpaths that don’t exist
  in the filesystem
- `unused_runpaths`: return a list of runpaths that are listed in the
  elf binary header, but no dependency was actually found in them
- `collect_unused_runpaths`: give an overview of all unused runpaths

Helpers:
- `dict_remove_empty`: remove fields with empty lists/dicts from an output
- `items`: `dict.iteritems()` for both python 2 and 3

See the introductory tutorial below on how to use these functions.

## Example usage

### Setup

If you have a bazel target which outputs a binary which you want to
debug, the easiest way is to use `ldd_test`:

```python
load(
    "//:debug/linking_utils/ldd_test.bzl",
    "ldd_test",
)

ldd_test(
    name = "test-ldd",
    elf_binary = "//tests/binary-indirect-cbits",
    current_workspace = None,
    script = r'''
YOUR SCRIPT HERE
'''
)
```

All exported functions from `ldd.py` are already in scope.
See the [`BUILD`](./BUILD) file in this directory for an example.


### Writing queries

`ldd` takes a function that is applied to each layer of elf
dependencies. This function is passed a set of structured data.
This data is gathered by querying the elf binary with `objdump`
and parsing the header fields of the dynamic section:

```
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
```

The amount of data can get quite extensive for larger projects, so you
need a way to filter it down to get to the bottom of our problem.

If a transitive dependency cannot be found by the runtime linker, the
binary cannot be started. `ldd` shows such a problem by setting
the corresponding value in the `needed` dict to `LDD_MISSING`.
To remove everything from the output but the missing dependency and
the path to that dependency, you can write a filter like this:

```python
# `d` is the DependencyInfo dict from above
def filter_down_to_missing(d):
    res = {}

    # items is a .iteritems() that works for py 2 and 3
    for name, dep in items(d['needed']):
        if dep == LDD_MISSING:
            res[name] = LDD_MISSING
        elif dep in LDD_ERRORS:
            pass
        else:
            # dep['item'] contains the already converted info
            # from the previous layer
            res[name] = dep['item']

    # dict_remove_empty removes all empty fields from the dict,
    # otherwise your result contains a lot of {} in the values.
    return dict_remove_empty(res)

# To get human-readable output, we re-use python’s pretty printing
# library. It’s only simple python values after all!
import pprint
pprint.pprint(
  # actually parse the elf binary and apply only_missing on each layer
  ldd(
    filter_down_to_missing,
    # the path to the elf binary you want to expect.
    elf_binary_path
  )
)
```

Note that in the filter you only need to filter the data for the
current executable, and add the info from previous layers (which are
available in `d['item']`).

The result might look something like:

```python
{'libfoo.so.5': {'libbar.so.1': {'libbaz.so.6': 'MISSING'}}}
```

or

```python
{}
```

if nothing is missing.

Now, that is a similar output to what a tool like `lddtree(1)` could
give you. But we don’t need to stop there because it’s trivial to
augment your output with more information:


```python
def missing_with_runpath(d):
  # our previous function can be re-used
  missing = filter_down_to_missing(d)

  # only display runpaths if there are missing deps
  runpaths = [] if missing is {} else d['runpath_dirs']

  # dict_remove_empty keeps the output clean
  return dict_remove_empty({
    'rpth': runpaths,
    'miss': missing
  })

# same invocation, different function
pprint.pprint(
  ldd(
    missing_with_runpath,
    elf_binary_path
  )
)
```

which displays something like this for my example binary:

```python
{ 'miss': { 'libfoo.so.5': { 'miss': { 'libbar.so.1': { 'miss': { 'libbaz.so.6': 'MISSING'},
                                                          'rpth': [ { 'absolute_path': '/home/philip/.cache/bazel/_bazel_philip/fd9fea5ad581ea59473dc1f9d6bce826/execroot/myproject/bazel-out/k8-fastbuild/bin/something/and/bazel-out/k8-fastbuild/bin/other/integrate',
                                                                      'path': '$ORIGIN/../../../../../../bazel-out/k8-fastbuild/bin/other/integrate'}]}},
                             'rpth': [ { 'absolute_path': '/nix/store/xdsjx0gba4id3yyqxv66bxnm2sqixkjj-glibc-2.27/lib',
                                         'path': '/nix/store/xdsjx0gba4id3yyqxv66bxnm2sqixkjj-glibc-2.27/lib'},
                                       { 'absolute_path': '/nix/store/x6inizi5ahlyhqxxwv1rvn05a25icarq-gcc-7.3.0-lib/lib',
                                         'path': '/nix/store/x6inizi5ahlyhqxxwv1rvn05a25icarq-gcc-7.3.0-lib/lib'}]}},
  'rpth': [ … lots more nix rpaths … ]}
```

That’s still a bit cluttered for my taste, so let’s filter out
the `/nix/store` paths (which are mostly noise):

```python
import re
nix_matcher = re.compile("/nix/store.*")

def missing_with_runpath(d):
  missing = filter_down_to_missing(d)

  # this is one of the example functions provided by ldd.py
  remove_matching_runpaths(d, nix_matcher)
  # ^^^

  runpaths = [] if missing is {} else d['runpath_dirs']

  # dict_remove_empty keeps the output clean
  return dict_remove_empty({
    'rpth': runpaths,
    'miss': missing
  })
```

and we are down to:

```python
{ 'miss': { 'libfoo.so.5': { 'miss': { 'libbar.so.1': { 'miss': { 'libbaz.so.6': 'MISSING'},
                                                          'rpth': [ { 'absolute_path': '/home/philip/.cache/bazel/_bazel_philip/fd9fea5ad581ea59473dc1f9d6bce826/execroot/myproject/bazel-out/k8-fastbuild/bin/something/and/bazel-out/k8-fastbuild/bin/other/integrate',
                                                                      'path': '$ORIGIN/../../../../../../bazel-out/k8-fastbuild/bin/other/integrate'}]}}}
```

… which shows exactly the path that is missing the dependency we
expect. But what has gone wrong? Does this path even exist? We can
find out!

```python
import re
nix_matcher = re.compile("/nix/store.*")

def missing_with_runpath(d):
  missing = filter_down_to_missing(d)
  remove_matching_runpaths(d, nix_matcher)
  runpaths = [] if missing is {} else d['runpath_dirs']

  # returns a list of runpaths that don’t exist in the filesystem
  doesnt_exist = non_existing_runpaths(d)
  # ^^^

  return dict_remove_empty({
    'rpth': runpaths,
    'miss': missing,
    'doesnt_exist': doesnt_exist,
  })
```

I amended the output by a list of runpaths which point to non-existing
directories:

```python
{ 'miss': { 'libfoo.so.5': { 'miss': { 'libbar.so.1': { 'miss': { 'libbaz.so.6': 'MISSING'},
                                                        'rpth': [ { 'absolute_path': '/home/philip/.cache/bazel/_bazel_philip/fd9fea5ad581ea59473dc1f9d6bce826/execroot/myproject/bazel-out/k8-fastbuild/bin/something/and/bazel-out/k8-fastbuild/bin/other/integrate',
                                                                    'path': '$ORIGIN/../../../../../../bazel-out/k8-fastbuild/bin/other/integrate'}]
                                                        'doesnt_exist': [ { 'absolute_path': '/home/philip/.cache/bazel/_bazel_philip/fd9fea5ad581ea59473dc1f9d6bce826/execroot/myproject/bazel-out/k8-fastbuild/bin/something/and/bazel-out/k8-fastbuild/bin/other/integrate',
                                                                            'path': '$ORIGIN/../../../../../../bazel-out/k8-fastbuild/bin/other/integrate'}]}}}
```

Suddenly it’s perfectly clear where the problem lies,
`$ORIGIN/../../../../../../bazel-out/k8-fastbuild/bin/other/integrate`
points to a path that does not exist.

Any data query you’d like to do is possible, as long as it uses
the data provided by the `ldd` function. See the lower part of
`ldd.py` for more examples.

