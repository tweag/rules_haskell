# features sh_inline_test and py_inline_test,
# which are like their respective builtin rules,
# but their scripts can be given inline as a string.

load("@bazel_skylib//lib:shell.bzl", "shell")

def quote_make_variables(s):
    """Quote all genrule “Make” Variables in a string."""
    return s.replace("$", "$$")

def target_from_string(name, string):
    """Write a skylark string to a target."""
    native.genrule(
        name = name + "-file",
        outs = [name],
        # this is exceptionally ugly.
        cmd = """echo -n {quoted} > $(@)""".format(
            # but should at least be quoted right
            quoted = shell.quote(quote_make_variables(string)),
        ),
    )

bash_runfiles_boilerplate = """\
# Copy-pasted from Bazel's Bash runfiles library (tools/bash/runfiles/runfiles.bash).
set -euo pipefail
if [[ ! -d "${RUNFILES_DIR:-/dev/null}" && ! -f "${RUNFILES_MANIFEST_FILE:-/dev/null}" ]]; then
  if [[ -f "$0.runfiles_manifest" ]]; then
    export RUNFILES_MANIFEST_FILE="$0.runfiles_manifest"
  elif [[ -f "$0.runfiles/MANIFEST" ]]; then
    export RUNFILES_MANIFEST_FILE="$0.runfiles/MANIFEST"
  elif [[ -f "$0.runfiles/bazel_tools/tools/bash/runfiles/runfiles.bash" ]]; then
    export RUNFILES_DIR="$0.runfiles"
  fi
fi
if [[ -f "${RUNFILES_DIR:-/dev/null}/bazel_tools/tools/bash/runfiles/runfiles.bash" ]]; then
  source "${RUNFILES_DIR}/bazel_tools/tools/bash/runfiles/runfiles.bash"
elif [[ -f "${RUNFILES_MANIFEST_FILE:-/dev/null}" ]]; then
  source "$(grep -m1 "^bazel_tools/tools/bash/runfiles/runfiles.bash " \
            "$RUNFILES_MANIFEST_FILE" | cut -d ' ' -f 2-)"
else
  echo >&2 "ERROR: cannot find @bazel_tools//tools/bash/runfiles:runfiles.bash"
  exit 1
fi
# --- end runfiles.bash initialization ---
"""

def sh_inline_test(name, script, **kwargs):
    """Like sh_test, but instead of srcs takes the shell script
    as verbatim bazel string. The bash runfiles are in scope,
    using `rlocation` works by default.
    """
    script_name = name + ".sh"
    script = bash_runfiles_boilerplate + script

    target_from_string(script_name, script)

    deps = kwargs.pop("deps", [])

    native.sh_test(
        name = name,
        srcs = [script_name],
        deps = ["@bazel_tools//tools/bash/runfiles"] + deps,
        **kwargs
    )

python_runfiles_boilerplate = """
from bazel_tools.tools.python.runfiles import runfiles
r = runfiles.Create()
"""

def py_inline_test(name, script, **kwargs):
    """Like py_test, but instead of srcs takes the shell script
    as verbatim bazel string. The python runfiles are in scope
    as the `r` variable. Use `r.Rlocation()`
    """
    script_name = name + ".py"
    script = python_runfiles_boilerplate + script

    target_from_string(script_name, script)

    deps = kwargs.pop("deps", [])
    srcs = kwargs.pop("srcs", [])

    native.py_test(
        name = name,
        srcs = [script_name] + srcs,
        main = script_name,
        deps = ["@bazel_tools//tools/python/runfiles"] + deps,
        **kwargs
    )
