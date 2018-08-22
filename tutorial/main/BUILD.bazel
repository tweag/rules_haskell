haskell_import(name = "base")

haskell_binary(
    name = "demorgan",
    srcs = ["Main.hs"],
    compiler_flags = ["-threaded"],
    deps = [":base", "//lib:booleans"],
)
