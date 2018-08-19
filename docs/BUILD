load("//skylark:lint.bzl", "skylark_lint")

genrule(
    name = "docs",
    srcs = ["conf.py"] + glob(["*.rst"]),
    outs = ["docs.zip"],
    cmd = """
    set -euo pipefail
    sourcedir=$$(dirname $(location conf.py))
    builddir=$$(mktemp -d rules_haskell_docs.XXXX)
    sphinx-build -M html $$sourcedir $$builddir -W -N -q
    (CWD=`pwd` && cd $$builddir/html && zip -q -r $$CWD/$@ .)
    rm -rf $$builddir
    """,
)

skylark_lint()
