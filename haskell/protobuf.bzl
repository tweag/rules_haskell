"""Support for protocol buffers"""

load(":providers.bzl", "HaskellBuildInfo", "HaskellProtobufInfo")
load("@bazel_skylib//:lib.bzl", "dicts", "paths")
load(":tools.bzl", "protobuf_tools")
load(":path_utils.bzl", "target_unique_name")

def _capitalize_first_letter(c):
  """Capitalize the first letter of the input. Unlike the built-in
  `capitalize()` method, doesn't lower-case the other characters. This helps
  mimic the behavior of `proto-lens-protoc`, which turns `Foo/Bar/BAZ.proto`
  into `Foo/Bar/BAZ.hs` (rather than `Foo/Bar/Baz.hs`).

  Args:
    c: A non-empty string word.

  Returns:
    The input with the first letter upper-cased.
  """
  return c[0].capitalize() + c[1:]

def _camel_case(comp):
  """Camel-case the input string, preserving any existing capital letters.
  """
  # Split on both "-" and "_", matching the behavior of proto-lens-protoc.
  return "".join([_capitalize_first_letter(c2)
                  for c1 in comp.split("_") for c2 in c1.split("-")])

def _proto_lens_output_file(path):
  """The output file from `proto-lens-protoc` when run on the given `path`.
  """

  path = path[:-len(".proto")]
  result = "/".join([_camel_case(p) for p in path.split("/")]) + ".hs"

  return "Proto/" + result

def _haskell_proto_aspect_impl(target, ctx):

  args = ctx.actions.args()

  src_prefix = paths.join(
    ctx.label.workspace_root,
    ctx.label.package,
  )

  args.add(["--plugin=protoc-gen-haskell=" +
            protobuf_tools(ctx).protoc_gen_haskell.path,
  ])

  hs_files = []
  inputs = []

  args.add(["-I{0}={1}".format(s.short_path, s.path)
            for s in target.proto.transitive_sources])

  inputs.extend(target.proto.transitive_sources.to_list())

  for src in target.proto.direct_sources:

    inputs.append(src)

    # As with the native rules, require the .proto file to be in the same
    # Bazel package as the proto_library rule. This allows us to put the
    # output .hs file next to the input .proto file. Unfortunately Skylark
    # doesn't let us check the package of the file directly, so instead we
    # just look at its short_path and rely on the proto_library rule itself
    # to check for consistency. We use the file's short_path rather than its
    # dirname/basename in case it's in a subdirectory; for example, if the
    # proto_library rule is in "foo/BUILD" but the .proto file is
    # "foo/bar/baz.proto".

    if not src.short_path.startswith(src_prefix):
      fail("Mismatch between rule context " + str(ctx.label.package)
           + " and source file " + src.short_path)
    if src.basename[-6:] != ".proto":
      fail("bad extension for proto file " + src)

    relative_path = src.short_path

    args.add([relative_path])

    hs_files.append(ctx.actions.declare_file(
      _proto_lens_output_file(relative_path),
    ))

  args.add([
    "--haskell_out=no-reexports:" + paths.join(
      hs_files[0].root.path,
      src_prefix,
    ),
  ])

  ctx.actions.run(
    inputs = depset([
      protobuf_tools(ctx).protoc,
      protobuf_tools(ctx).protoc_gen_haskell,
    ] + inputs),
    outputs = hs_files,
    executable = protobuf_tools(ctx).protoc,
    arguments = [args],
  )

  hs_files_depset = depset(hs_files)

  # Collect the generated source files from proto rule's dependencies.
  for dep in ctx.rule.attr.deps:
    hs_files_depset = depset(transitive = [
      hs_files_depset,
      dep[HaskellProtobufInfo].source_files,
    ])

  return [HaskellProtobufInfo(
    source_files = hs_files_depset,
  )]

haskell_proto_aspect = aspect(
  _haskell_proto_aspect_impl,
  attr_aspects = ['deps'],
  toolchains = ["@io_tweag_rules_haskell//protobuf:toolchain"],
)

def _protobuf_toolchain_impl(ctx):
  return [
    platform_common.ToolchainInfo(
      name = ctx.label.name,
      tools = struct(
        protoc = ctx.executable.protoc,
        protoc_gen_haskell = ctx.executable.protoc_gen_haskell,
      ),
      haskell_deps = ctx.attr.haskell_deps,
      haskell_prebuilt_deps = ctx.attr.haskell_prebuilt_deps,
    )
  ]

_protobuf_toolchain = rule(
  _protobuf_toolchain_impl,
  host_fragments = ["cpp"],
  attrs = {
    "protoc": attr.label(
      executable = True,
      cfg = "host",
      allow_single_file = True,
      mandatory = True,
      doc = "protoc compiler",
    ),
    "protoc_gen_haskell": attr.label(
      executable = True,
      cfg = "host",
      allow_single_file = True,
      mandatory = True,
      doc = "proto-lens-protoc plugin for protoc",
    ),
    "haskell_deps": attr.label_list(
      doc = "List of other Haskell libraries to be linked to protobuf libraries.",
    ),
    "haskell_prebuilt_deps": attr.string_list(
      doc = "Non-Bazel supplied Cabal dependencies for protobuf libraries.",
    ),
  },
)

def haskell_proto_toolchain(
    name,
    protoc_gen_haskell,
    haskell_deps=[],
    haskell_prebuilt_deps=[],
    protoc=Label("@com_google_protobuf//:protoc"),
    **kwargs):
  """Declare a Haskell protobuf toolchain.

  You need at least one of these declared somewhere in your `BUILD` files
  for the `haskell_proto_library` rules to work. Once declared, you then
  need to *register* the toolchain using `register_toolchain` in your
  `WORKSPACE` file (see example below).

  Example:

    In a `BUILD` file:

    ```bzl
    haskell_toolchain(
      name = "protobuf-toolchain",
      protoc = "@com_google_protobuf//:protoc",
      protoc_gen_haskell = "@protoc_gen_haskell//:bin/proto-lens-protoc",
      haskell_prebuilt_deps = [
        "base",
        "bytestring",
        "containers",
        "data-default-class",
        "lens-family",
        "lens-labels",
        "proto-lens",
        "text",
      ],
    )
    ```

    The `haskell_prebuilt_deps` and `haskell_deps` arguments allow to
    specify Haskell libraries to use to compile the auto-generated source
    files.

    In `WORKSPACE` you could have something like this:

    ```bzl
    http_archive(
      name = "com_google_protobuf",
      sha256 = "cef7f1b5a7c5fba672bec2a319246e8feba471f04dcebfe362d55930ee7c1c30",
      strip_prefix = "protobuf-3.5.0",
      urls = ["https://github.com/google/protobuf/archive/v3.5.0.zip"],
    )

    nixpkgs_package(
      name = "protoc_gen_haskell",
      repository = "@nixpkgs",
      attribute_path = "haskell.packages.ghc822.proto-lens-protoc
    )

    register_toolchains(
      "//tests:ghc", # assuming you called your Haskell toolchain \"ghc\"
      "//tests:protobuf-toolchain",
    )
    ```
  """
  impl_name = name + "-impl"
  _protobuf_toolchain(
    name = impl_name,
    protoc_gen_haskell = protoc_gen_haskell,
    haskell_deps = haskell_deps,
    haskell_prebuilt_deps = haskell_prebuilt_deps,
    protoc = protoc,
    visibility = ["//visibility:public"],
    **kwargs
  )

  native.toolchain(
    name = name,
    toolchain_type = "@io_tweag_rules_haskell//protobuf:toolchain",
    toolchain = ":" + impl_name,
    exec_compatible_with = [
      "@bazel_tools//platforms:x86_64",
    ],
    target_compatible_with = [
      "@bazel_tools//platforms:x86_64",
    ],
  )
