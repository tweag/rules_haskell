# Debugging linking errors

The usual utilties like `nm`, `objdump` and of course `ldd` (see
[here](https://linux-audit.com/elf-binaries-on-linux-understanding-and-analysis/#tools-for-binary-analysis)
for a good overview of existing tools) go a long way, but when
debugging non-trivial runtime linker failures one would often like to
filter outputs programmatically with more than just simple `grep` and
`sed` expressions.

This library provides a small set of utility subroutines that can help
debug complicated linker errors.

The main function is `ldd(f, elf_path)`, which is in the same spirit
as `ldd(1)`, but returns a tree of shared dependencies instead of a
flat list. Additionally, it expects a function `f` which is applied to
each recursion level of dependencies.

Functions that can be passed to `ldd`:

- `identity`: pass through every info `ldd` can output
- `remove_uninteresting_dependencies`: remove entries that are mostly noise
- `was_runpath_used`: return a list of unused runpaths

Helpers:
- `dict_remove_empty`: remove fields with empty lists/dicts from an output

Example usage:

```python
import pprint
from bazel_tools.tools.python.runfiles import runfiles
from debug.linking_utils.ldd import \
  ldd, \
  was_runpath_used, \
  dict_remove_empty, \
  remove_uninteresting_dependencies

r = runfiles.Create()
pp = pprint.PrettyPrinter(indent=2)
path = r.Rlocation("my_workspace/path/to/my/binary.so")

pp.pprint(
  ldd(remove_uninteresting_dependencies, path)
)

print("\nUnused RUNPATH entries:")
pp.pprint(
  dict_remove_empty(
    ldd(was_runpath_used, path)['others']
  )
)
```
