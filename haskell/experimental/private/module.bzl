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
    "//haskell:private/mode.bzl",
    "is_profiling_enabled",
)
load(
    "//haskell:private/packages.bzl",
    "expose_packages",
    "pkg_info_to_compile_flags",
)
load(
    "//haskell/experimental:providers.bzl",
    "HaskellModuleInfo",
)
load(
    "@bazel_skylib//rules:common_settings.bzl",
    "BuildSettingInfo",
)

def haskell_module_impl(ctx):
    # Obtain toolchains
    hs = haskell_context(ctx)
    cc = cc_interop_info(ctx)
    posix = ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]

    # Collect dependencies
    src = ctx.file.src
    extra_srcs = ctx.files.extra_srcs
    dep_info = gather_dep_info(ctx, ctx.attr.deps)
    # TODO[AH] Support plugins
    # ctx.attr.plugins
    # TODO[AH] Support preprocessors
    # ctx.attr.tools

    # Determine outputs
    with_profiling = is_profiling_enabled(hs)
    hs_boot = paths.split_extension(src.path)[1] == ".hs-boot"
    extension_template = "%s"
    if hs_boot:
        extension_template = extension_template + "-boot"
    if with_profiling:
        extension_template = "p_" + extension_template
    extension_template = "." + extension_template

    obj = ctx.actions.declare_file(
        paths.replace_extension(src.basename, extension_template % "o"),
        sibling = src,
    )
    interface = ctx.actions.declare_file(
        paths.replace_extension(src.basename, extension_template % "hi"),
        sibling = src,
    )

    # TODO[AH] Support additional outputs such as `.hie`.

    # Construct compiler arguments

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
    if with_profiling:
        args.add_all([
            "-prof",
            "-fexternal-interpreter",
            "-hisuf",
            "p_hi",
            "-osuf",
            "p_o",
        ])
    package_name = getattr(ctx.attr, "package_name", None)
    if not package_name:
        package_name = ctx.attr._package_name_setting[BuildSettingInfo].value
    if package_name != "":
        args.add_all([
            "-this-unit-id",
            package_name,
            "-optP-DCURRENT_PACKAGE_KEY=\"{}\"".format(ctx.attr.package_name),
        ])
    if not hs.toolchain.is_windows:
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

    # Collect module dependency arguments
    args.add_all([
        # TODO[AH] Factor this out
        # TODO[AH] Include object search paths for template Haskell dependencies.
        #   See https://github.com/tweag/rules_haskell/issues/1382
        dep[HaskellModuleInfo].import_dir
        for dep in ctx.attr.deps
        if HaskellModuleInfo in dep
    ], format_each = "-i%s")

    # Collect library dependency arguments
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

    # Compile the module
    hs.toolchain.actions.run_ghc(
        hs,
        cc,
        inputs = depset(
            direct = [src] + extra_srcs + [optp_args_file],
            transitive = [pkg_info_inputs] + [
                # TODO[AH] Factor this out
                # TODO[AH] Include object files for template Haskell dependencies.
                depset(direct = [dep[HaskellModuleInfo].interface_file])
                for dep in ctx.attr.deps
                if HaskellModuleInfo in dep
            ],
        ),
        input_manifests = [],
        outputs = [obj, interface],
        mnemonic = "HaskellBuildObject" + ("Prof" if with_profiling else ""),
        progress_message = "HaskellBuildObject {}".format(hs.label),
        env = hs.env,
        arguments = args,
    )

    # Construct the import search paths for this module
    workspace_root = paths.join(ctx.bin_dir.path, ctx.label.workspace_root)
    package_root = paths.join(workspace_root, ctx.label.package)
    src_strip_prefix = ctx.attr.src_strip_prefix
    if src_strip_prefix.startswith("/"):
        import_dir = paths.join(workspace_root, src_strip_prefix[1:])
    else:
        import_dir = paths.join(package_root, src_strip_prefix)

    # Construct and return providers

    default_info = DefaultInfo(
        files = depset(direct = [obj, interface]),
    )
    module_info = HaskellModuleInfo(
        object_file = obj,
        import_dir = import_dir,
        interface_file = interface,
    )

    return [default_info, module_info]
