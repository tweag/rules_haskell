"""c2hs support"""

load(":private/tools.bzl",
     "get_build_tools_path",
     "tools",
)
load(":cc.bzl",
     "cc_headers",
)

load("@bazel_skylib//:lib.bzl", "paths")

def _haskell_c2hs_impl(ctx):

  args = ctx.actions.args()

  # some common options
  args.add(["-C-E"])

  h = cc_headers(ctx)
  common_inputs = h.hdrs

  # cc executable
  args.add(["--cpp", tools(ctx).gcc.path])
  common_inputs.append(tools(ctx).gcc)
  # detected cpp flags
  args.add(["-C" + x for x in h.cpp_flags])
  # detected include args
  args.add(["-C" + x for x in h.include_args])
  # user-supplied C options
  args.add(["-C" + x for x in ctx.attr.copts])

  outputs = []
  for src in ctx.files.srcs:

    output = ctx.actions.declare_file(
      paths.relativize(
        paths.replace_extension(src.path, ".hs"),
        paths.join(
          src.root.path,
          ctx.label.workspace_root,
          ctx.label.package,
        ),
      )
    )

    ctx.actions.run(
      inputs = depset(transitive = [
        depset(common_inputs),
        depset([src]),
      ]),
      outputs = [output],
      executable = tools(ctx).c2hs,
      mnemonic = "HaskellC2Hs",
      arguments = [args, "-o", output.path, src.path],
    )

    outputs.append(output)

  return DefaultInfo(files=depset(outputs))

haskell_c2hs = rule(
  _haskell_c2hs_impl,
  attrs = {
    "srcs": attr.label_list(
      mandatory=True,
      allow_files=True,
      doc="Input `.chs` files.",
    ),
    "deps": attr.label_list(
      providers=["cc"],
      doc="List of `cc_library` rules providing headers needed by the `.chs` files.",
    ),
    "copts": attr.string_list(
      doc="List of c options to be passed to c2hs prefixed with `-C`.",
    ),
  },
  output_to_genfiles = True,  # Needed to work with haskell rules.
  toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)
"""The rule produces Haskell sources using `c2hs` which then can be fed into
`haskell_library` along with normal source files.

For example:
  ```bzl
  haskell_c2hs(
    name = "c2hs",
    srcs = [
      "Foo.chs",
      "Bar.chs",
    ],
  )

  haskell_library(
    name = "c2hs-consumer",
    srcs = [
      "Baz.hs",
      ":c2hs", # i.e. Foo.hs and Bar.hs
    ],
    prebuilt_dependencies = [
      "base",
    ],
  )
  ```
"""
