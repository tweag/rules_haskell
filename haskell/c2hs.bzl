"""Support for c2hs"""

load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    ":providers.bzl",
    "C2hsLibraryInfo",
)
load(":cc.bzl", "cc_interop_info")
load(
    ":private/cc_libraries.bzl",
    "haskell_cc_libraries_aspect",
)
load(":private/context.bzl", "haskell_context")
load(":private/dependencies.bzl", "gather_dep_info")
load(
    ":private/path_utils.bzl",
    "declare_compiled",
    "target_unique_name",
)
load(":private/set.bzl", "set")
load(":private/version_macros.bzl", "version_macro_includes")

def _c2hs_library_impl(ctx):
    hs = haskell_context(ctx)

    cc = cc_interop_info(ctx)
    args = hs.actions.args()
    c2hs = ctx.toolchains["@rules_haskell//haskell/c2hs:toolchain"].c2hs
    c2hs_exe = ctx.toolchains["@rules_haskell//haskell/c2hs:toolchain"].c2hs_exe

    if len(ctx.files.srcs) != 1:
        fail("srcs field should contain exactly one file.")
    chs_file = ctx.files.srcs[0]

    # Output a Haskell source file.
    chs_dir_raw = target_unique_name(hs, "chs")
    hs_file = declare_compiled(hs, chs_file, ".hs", directory = chs_dir_raw)
    chi_file = declare_compiled(hs, chs_file, ".chi", directory = chs_dir_raw)
    args.add_all([chs_file.path, "-o", hs_file.path])

    args.add("-C-E")
    args.add_all(["--cpp", cc.tools.cpp])
    args.add("-C-includeghcplatform.h")
    args.add("-C-includeghcversion.h")
    args.add_all(["-C" + x for x in cc.cpp_flags])
    args.add_all(["-C" + x for x in cc.include_args])
    args.add_all(ctx.attr.extra_args)

    dep_chi_files = [
        dep[C2hsLibraryInfo].chi_file
        for dep in ctx.attr.deps
        if C2hsLibraryInfo in dep
    ]

    chi_includes = [
        "-i" + dep[C2hsLibraryInfo].import_dir
        for dep in ctx.attr.deps
        if C2hsLibraryInfo in dep
    ]
    args.add_all(chi_includes)

    version_macro_headers = set.empty()
    if ctx.attr.version:
        dep_info = gather_dep_info(ctx, ctx.attr.deps)
        (version_macro_headers, version_macro_flags) = version_macro_includes(dep_info)
        args.add_all(["-C" + x for x in version_macro_flags])

    (inputs, input_manifests) = ctx.resolve_tools(tools = [c2hs])

    hs.actions.run_shell(
        inputs = depset(transitive = [
            depset(cc.hdrs),
            depset([chs_file]),
            depset(dep_chi_files),
            depset(cc.files),
            set.to_depset(version_macro_headers),
            inputs,
        ]),
        input_manifests = input_manifests,
        tools = [hs.tools.ghc, c2hs_exe],
        outputs = [hs_file, chi_file],
        command =
            # cpp (called via c2hs) gets very unhappy if the mingw bin dir is
            # not in PATH so we add it to PATH explicitely.
            (
                """
        export PATH=$PATH:{mingw_bin}
        """.format(mingw_bin = paths.dirname(cc.tools.cc)) if hs.toolchain.is_windows else ""
            ) +
            """
        # Include libdir in include path just like hsc2hs does.
        libdir=$({ghc} --print-libdir)
        {c2hs} -C-I$libdir/include "$@"
        """.format(
                ghc = hs.tools.ghc.path,
                c2hs = c2hs_exe.path,
            ),
        mnemonic = "HaskellC2Hs",
        arguments = [args],
        env = hs.env,
    )

    idir = paths.join(
        hs.bin_dir.path,
        hs.label.workspace_root,
        hs.label.package,
        chs_dir_raw,
    )

    return [
        DefaultInfo(files = depset([hs_file])),
        C2hsLibraryInfo(
            chi_file = chi_file,
            import_dir = idir,
        ),
    ]

c2hs_library = rule(
    _c2hs_library_impl,
    attrs = {
        "deps": attr.label_list(
            aspects = [haskell_cc_libraries_aspect],
        ),
        "srcs": attr.label_list(allow_files = [".chs"]),
        "extra_args": attr.string_list(
            doc = "Extra arguments that should be passedto c2hs.",
        ),
        "src_strip_prefix": attr.string(
            doc = "Directory in which module hierarchy starts.",
        ),
        "version": attr.string(
            doc = "Executable version. If this is specified, CPP version macros will be generated for this build.",
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
    },
    toolchains = [
        "@bazel_tools//tools/cpp:toolchain_type",
        "@rules_haskell//haskell:toolchain",
        "@rules_haskell//haskell/c2hs:toolchain",
    ],
    fragments = ["cpp"],
)

def _c2hs_toolchain_impl(ctx):
    return [
        platform_common.ToolchainInfo(
            name = ctx.label.name,
            # We have both c2hs which points to the target and c2hs_exe
            # which points to the file. The former is used to collect
            # runfiles while the latter is used to get the path to
            # c2hs.
            c2hs = ctx.attr.c2hs,
            c2hs_exe = ctx.executable.c2hs,
        ),
    ]

_c2hs_toolchain = rule(
    _c2hs_toolchain_impl,
    attrs = {
        "c2hs": attr.label(
            doc = "The c2hs executable.",
            mandatory = True,
            allow_single_file = True,
            executable = True,
            cfg = "host",
        ),
    },
)

def c2hs_toolchain(name, c2hs, **kwargs):
    """Declare a Haskell c2hs toolchain.

    You need at least one of these declared somewhere in your `BUILD`
    files for the `chs_library` rule to work. Once declared, you then
    need to *register* the toolchain using `register_toolchains` in
    your `WORKSPACE` file (see example below).

    ### Examples

      In a `BUILD` file:

      ```bzl
      c2hs_toolchain(
          name = "c2hs",
          c2hs = "@c2hs//:bin",
      )
      ```

      where `@c2hs` is an external repository defined in the
      `WORKSPACE`, e.g. using:

      ```bzl
      nixpkgs_package(
          name = "c2hs",
          attribute_path = "haskell.packages.ghc822.c2hs",
      )

      register_toolchains("//:c2hs")
      ```
    """
    impl_name = name + "-impl"
    _c2hs_toolchain(
        name = impl_name,
        c2hs = c2hs,
        visibility = ["//visibility:public"],
        **kwargs
    )

    native.toolchain(
        name = name,
        toolchain_type = "@rules_haskell//haskell/c2hs:toolchain",
        toolchain = ":" + impl_name,
    )
