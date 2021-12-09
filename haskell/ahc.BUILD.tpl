load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_import",
    "haskell_toolchain",
)

load(
    "@rules_haskell//haskell/asterius:defs.bzl",
    "asterius_toolchain",
)

package(default_visibility = ["//visibility:public"])

%{toolchain_libraries}

%{toolchain}

%{asterius_toolchain}
