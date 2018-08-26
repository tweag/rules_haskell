load("@io_bazel_skydoc//skylark:skylark.bzl", "skylark_doc")
load("//skylark:lint.bzl", "skylark_lint")

genrule(
    name = "guide_html",
    srcs = ["conf.py"] + glob(["*.rst"]),
    outs = ["guide_html.zip"],
    cmd = """
    set -euo pipefail
    sourcedir=$$(dirname $(location conf.py))
    builddir=$$(mktemp -d rules_haskell_docs.XXXX)
    sphinx-build -M html $$sourcedir $$builddir -W -N -q
    (CWD=`pwd` && cd $$builddir/html && zip -q -r $$CWD/$@ .)
    rm -rf $$builddir
    """,
)

skylark_doc(
    name = "api_html",
    srcs = [

        # The order of these files defines the order in which the corresponding
        # sections are presented in the docs.
        "//haskell:haskell.bzl",
        "//haskell:haddock.bzl",
        "//haskell:lint.bzl",
        "//haskell:toolchain.bzl",
        "//haskell:protobuf.bzl",
        "//haskell:cc.bzl",
        "//haskell:repositories.bzl",
        "//haskell:ghc_bindist.bzl",
    ],
    format = "html",
)

skylark_lint()
