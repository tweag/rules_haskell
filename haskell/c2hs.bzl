"""Support for c2hs"""

load(":cc.bzl", "cc_interop_info")
load(":private/context.bzl", "haskell_context")
load(
    ":private/path_utils.bzl",
    "declare_compiled",
    "target_unique_name",
)
load(
    ":private/providers.bzl",
    "C2hsLibraryInfo",
)
load("@bazel_skylib//:lib.bzl", "paths")

def _c2hs_library_impl(ctx):
    hs = haskell_context(ctx)
    cc = cc_interop_info(ctx)
    args = hs.actions.args()

    if len(ctx.files.srcs) != 1:
        fail("srcs field should contain exactly one file.")
    chs_file = ctx.files.srcs[0]

    # Output a Haskell source file.
    chs_dir_raw = target_unique_name(hs, "chs")
    hs_file = declare_compiled(hs, chs_file, ".hs", directory = chs_dir_raw)
    chi_file = declare_compiled(hs, chs_file, ".chi", directory = chs_dir_raw)
    args.add([chs_file.path, "-o", hs_file.path])

    args.add(["-C-E"])
    args.add(["--cpp", hs.tools.cc.path])
    args.add("-C-includeghcplatform.h")
    args.add("-C-includeghcversion.h")
    args.add(["-C" + x for x in cc.cpp_flags])
    args.add(["-C" + x for x in cc.include_args])

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
    args.add(chi_includes)

    hs.actions.run(
        inputs = depset(transitive = [
            depset(cc.hdrs),
            depset([hs.tools.cc, hs.tools.ghc, hs.tools.c2hs, chs_file]),
            depset(dep_chi_files),
        ]),
        outputs = [hs_file, chi_file],
        executable = ctx.file._chs_wrapper,
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
        "deps": attr.label_list(),
        "srcs": attr.label_list(allow_files = [".chs"]),
        "src_strip_prefix": attr.string(
            doc = "Directory in which module hierarchy starts.",
        ),
        "_chs_wrapper": attr.label(
            allow_single_file = True,
            default = Label("@io_tweag_rules_haskell//haskell:private/c2hs_wrapper.sh"),
        ),
    },
    toolchains = [
        "@io_tweag_rules_haskell//haskell:toolchain",
        "@bazel_tools//tools/cpp:toolchain_type",
    ],
)
