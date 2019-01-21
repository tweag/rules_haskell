package(default_visibility = ["//visibility:public"])

filegroup(
    name = "bin",
    srcs = glob([
        "bin/*",
        "mingw/bin/*",
    ]),
)

cc_library(
    name = "threaded-rts",
    srcs = glob(
        ["lib/ghc-*/rts/libHSrts_thr-ghc*." + ext for ext in [
            "so",
            "dylib",
        ]] +
        # dependency of `libHSrts_thr_ghc*`
        # globbing on the `so` version to stay working when they update
        [
            "lib/ghc-*/rts/libffi.so.*",
        ],
    ),
    hdrs = glob(["lib/ghc-*/include/**/*.h"]),
    strip_include_prefix = glob(
        ["lib/ghc-*/include"],
        exclude_directories = 0,
    )[0],
)
