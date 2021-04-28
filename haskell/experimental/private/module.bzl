load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    "//haskell:cc.bzl",
    "cc_interop_info",
)
load(
    "//haskell:private/context.bzl",
    "haskell_context",
)
load(
    "//haskell:private/dependencies.bzl",
    "gather_dep_info",
)
load(
    "//haskell:private/packages.bzl",
    "expose_packages",
    "pkg_info_to_compile_flags",
)

def haskell_module_impl(ctx):
    hs = haskell_context(ctx)
    cc = cc_interop_info(ctx)
    posix = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]

    src = ctx.file.src
    extra_srcs = ctx.files.extra_srcs
    dep_info = gather_dep_info(ctx, ctx.attr.deps)
    # ctx.attr.deps
    # TODO[AH] Support plugins
    # ctx.attr.plugins
    # TODO[AH] Support preprocessors
    # ctx.attr.tools

    obj = ctx.actions.declare_file(
        paths.replace_extension(src.basename, ".o"),
        sibling = src,
    )
    interface = ctx.actions.declare_file(
        paths.replace_extension(src.basename, ".hi"),
        sibling = src,
    )
    # TODO[AH] Support additional outputs such as `.hie`.

    args = ctx.actions.args()
    args.add_all(["-c", "-o", obj, "-ohi", interface, src])
    args.add_all([
        "-v0",
        "-fPIC",
        "-hide-all-packages",
        # Should never trigger in sandboxed builds, but can be useful
        # to debug issues in non-sandboxed builds.
        "-Wmissing-home-modules",
    ])
    if hs.toolchain.static_runtime and not hs.toolchain.is_windows:
        # A static GHC RTS requires -fPIC. However, on Unix we also require
        # -fexternal-dynamic-refs, otherwise GHC still generates R_X86_64_PC32
        # relocations which prevents loading these static libraries as PIC.
        args.add("-fexternal-dynamic-refs")

    # GHC expects the CC compiler as the assembler, but segregates the
    # set of flags to pass to it when used as an assembler. So we have
    # to set both -optc and -opta.
    args.add_all(cc.compiler_flags, format_each = "-optc%s")
    args.add_all(cc.compiler_flags, format_each = "-opta%s")

    # Write the -optP flags to a parameter file because they can be very long on Windows
    # e.g. 27Kb for grpc-haskell
    optp_args_file = hs.actions.declare_file("optp_args_%s" % hs.name)
    optp_args = hs.actions.args()
    optp_args.add_all(cc.cpp_flags)
    optp_args.set_param_file_format("multiline")
    hs.actions.write(optp_args_file, optp_args)
    args.add(optp_args_file, format = "-optP@%s")

    args.add_all(cc.include_args)

    (pkg_info_inputs, pkg_info_args) = pkg_info_to_compile_flags(
        hs,
        pkg_info = expose_packages(
            package_ids = hs.package_ids,
            package_databases = dep_info.package_databases,
            # TODO[AH] Support version macros
            version = None,
        ),
        # TODO[AH] Support plugins
        plugin_pkg_info = expose_packages(
            package_ids = [],
            package_databases = depset(),
            version = None,
        ),
        prefix = "compile-",
    )
    args.add_all(pkg_info_args)

    # TODO[AH] Support package id - see `-this-unit-id` flag.

    args.add_all(hs.toolchain.ghcopts)
    args.add_all(ctx.attr.ghcopts)

    hs.toolchain.actions.run_ghc(
        hs,
        cc,
        inputs = depset(
            direct = [src] + extra_srcs + [optp_args_file],
            transitive = [pkg_info_inputs],
        ),
        input_manifests = [],
        outputs = [obj, interface],
        # TODO[AH] Support profiling
        mnemonic = "HaskellBuildObject",  # + ("Prof" if with_profiling else ""),
        progress_message = "HaskellBuildObject {}".format(hs.label),
        env = hs.env,
        arguments = args,
    )

    default_info = DefaultInfo(
        files = depset(direct = [obj, interface]),
    )

    return [default_info]
