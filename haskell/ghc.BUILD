package(default_visibility = ["//visibility:public"])

filegroup(
  name = "bin",
  srcs = glob([
    "bin/*",
  ]),
)

cc_library(
  name = "threaded-rts",
  srcs = glob(["lib/ghc-*/rts/libHSrts_thr-ghc*.so"]),
  hdrs = glob(["lib/ghc-*/include/**/*.h"]),
  strip_include_prefix = glob(["lib/ghc-*/include"], exclude_directories=0)[0],
)
