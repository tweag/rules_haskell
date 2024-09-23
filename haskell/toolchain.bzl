"""Rules for defining toolchains"""

load("@bazel_skylib//lib:paths.bzl", "paths")
load("@rules_cc//cc:find_cc_toolchain.bzl", "find_cc_toolchain", "use_cc_toolchain")
load(
    "//haskell/asterius:asterius_config.bzl",
    "ASTERIUS_BINARIES",
    "asterius_tools_config",
)
load(":cc.bzl", "ghc_cc_program_args")
load(":ghc_bindist.bzl", "haskell_register_ghc_bindists")
load(
    ":private/actions/compile.bzl",
    "compile_binary",
    "compile_library",
)
load(
    ":private/actions/link.bzl",
    "link_binary",
    "link_library_dynamic",
    "link_library_static",
    "merge_parameter_files",
)
load(":private/actions/package.bzl", "package")
load(":private/context.bzl", "append_to_path")
load(
    ":private/haskell_impl.bzl",
    "HaskellImportHack",
    "HaskellToolchainLibraries",
)
load(
    ":private/path_utils.bzl",
    "get_dynamic_hs_lib_name",
    "get_lib_extension",
    "get_static_hs_lib_name",
)
load(
    ":providers.bzl",
    "HaddockInfo",
    "HaskellInfo",
    "HaskellLibraryInfo",
)

_GHC_BINARIES = ["ghc", "ghc-pkg", "hsc2hs", "haddock", "runghc", "hpc"]

def _toolchain_library_symlink(dynamic_library):
    prefix = dynamic_library.owner.workspace_root.replace("_", "_U").replace("/", "_S")
    basename = dynamic_library.basename
    return paths.join(prefix, basename)

def _run_ghc(
        hs,
        cc,
        inputs,
        outputs,
        mnemonic,
        arguments,
        env,
        params_file = None,
        progress_message = None,
        input_manifests = None,
        interface_inputs = [],
        extra_name = "",
        hi_file = None,
        abi_file = None):
    args = hs.actions.args()
    extra_inputs = []

    # Detect persistent worker support
    flagsfile_prefix = ""
    execution_requirements = {}
    tools = []
    if hs.worker != None:
        flagsfile_prefix = "@"
        execution_requirements = {"supports-workers": "1"}
        args.add(hs.worker.path)
        tools = [hs.worker]
    else:
        args.add(hs.tools.ghc)
        extra_inputs += [hs.tools.ghc]

    # XXX: We should also tether Bazel's CC toolchain to GHC's, so that we can properly mix Bazel-compiled
    # C libraries with Haskell targets.
    args.add_all(ghc_cc_program_args(hs, cc.tools.cc, cc.tools.ld))

    compile_flags_file = hs.actions.declare_file("compile_flags_%s_%s_%s" % (hs.name, extra_name, mnemonic))
    extra_args_file = hs.actions.declare_file("extra_args_%s_%s_%s" % (hs.name, extra_name, mnemonic))

    args.set_param_file_format("multiline")
    arguments.set_param_file_format("multiline")
    hs.actions.write(compile_flags_file, args)
    hs.actions.write(extra_args_file, arguments)

    if abi_file != None:
        # We declare the file containing informations about the call to GHC with --show-iface.
        show_iface_file = hs.actions.declare_file("show_iface_%s_%s" % (hs.name, extra_name))
        show_iface_args = hs.actions.args()
        show_iface_args.set_param_file_format("multiline")
        show_iface_args.add(hs.tools.ghc.path)
        show_iface_args.add("--show-iface ")
        show_iface_args.add(hi_file.path)
        hs.actions.write(show_iface_file, show_iface_args)

        # We create a file containing the name of all the interface files which should not be considered
        # by the caching mechanism to know if recompilation should be triggered.
        # This behaivour is extensively described in the Note [On the ABI hash] in haskell/experimaental/private/module.bzl
        interface_with_abis_list = hs.actions.declare_file("interfaces_%s_%s" % (hs.name, extra_name))
        interface_with_abis_args = hs.actions.args()
        interface_with_abis_args.set_param_file_format("multiline")
        interface_with_abis_args.add_all(interface_inputs)
        hs.actions.write(interface_with_abis_list, interface_with_abis_args)

        # The interface_with_abis_list should be used as unused_inputs_list.
        # However, since it is more traditional to generate the unused_inputs_list within the action itself,
        # and this proposition is already quite hackish, we wanted to stick to the usage,
        # expecting it to offer us a stronger reliability, even with the future evolution of Bazel.
        unused_inputs_list = hs.actions.declare_file("unused_%s_%s" % (hs.name, extra_name))

        extra_inputs += [show_iface_file, interface_with_abis_list]
        outputs += [unused_inputs_list]
        env.update({"MUST_EXTRACT_ABI": "true"})

        new_args = [show_iface_file.path, abi_file.path, interface_with_abis_list.path, unused_inputs_list.path]
    else:
        env.update({"MUST_EXTRACT_ABI": "false"})
        new_args = []
        unused_inputs_list = None

    extra_inputs += [
        compile_flags_file,
        extra_args_file,
    ] + cc.files + hs.toolchain.bindir + hs.toolchain.libdir

    if hs.toolchain.locale_archive != None:
        extra_inputs.append(hs.toolchain.locale_archive)

    flagsfile = extra_args_file
    if params_file:
        flagsfile = merge_parameter_files(hs, extra_args_file, params_file)
        extra_inputs.append(flagsfile)

    if type(inputs) == type(depset()):
        inputs = depset(extra_inputs, transitive = [inputs])
    else:
        inputs += extra_inputs

    if input_manifests != None:
        input_manifests = input_manifests + cc.manifests
    else:
        input_manifests = cc.manifests

    tools.extend(hs.tools_config.tools_for_ghc)
    append_to_path(env, hs.toolchain.is_windows, hs.tools_config.path_for_run_ghc)

    hs.actions.run(
        inputs = inputs,
        tools = tools,
        input_manifests = input_manifests,
        outputs = outputs,
        executable = hs.ghc_wrapper,
        mnemonic = mnemonic,
        progress_message = progress_message,
        env = env,
        arguments = [compile_flags_file.path, flagsfile_prefix + flagsfile.path] + new_args,
        execution_requirements = execution_requirements,
        unused_inputs_list = unused_inputs_list,
    )

    return args

default_tools_config = struct(
    path_for_run_ghc = [],
    tools_for_ghc = [],
    path_for_cabal = [],
    tools_for_ghc_pkg = [],

    # for cross compiling we will pass ghc a cross compiling cc_toolchain,
    # and not one targeting the exec platform.
    maybe_exec_cc_toolchain = None,
    supports_haddock = True,
)

def _lookup_binaries(names, files, version = ""):
    binaries = {}
    for tool in list(names):
        for file in files:
            basename_no_ext = paths.split_extension(file.basename)[0]
            if tool == basename_no_ext:
                binaries[tool] = file
                break
            if version and "%s-%s" % (tool, version) == basename_no_ext:
                binaries[tool] = file
                break
        if not tool in binaries:
            fail("Cannot find {} in {}".format(tool, files))
    return binaries

def _haskell_toolchain_libraries(ctx, libraries):
    with_profiling = ctx.var["COMPILATION_MODE"] == "dbg"  # TODO is_profiling_enabled(hs)
    with_threaded = "-threaded" in ctx.attr.ghcopts

    cc_toolchain = find_cc_toolchain(ctx)
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )

    # List of library in left-to-right post-ordering
    # Meaning, if package B depends on package A, then A will appear before B.
    ordered = depset(transitive = [
        target[HaskellImportHack].transitive_depends
        for target in libraries.values()
    ])

    library_dict = {}
    for package in ordered.to_list():
        target = libraries[package]

        # Construct CcInfo
        if with_profiling:
            # GHC does not provide dynamic profiling mode libraries. The dynamic
            # libraries that are available are missing profiling symbols, that
            # other profiling mode build results will reference. Therefore, we
            # don't import dynamic libraries in profiling mode.
            libs = {
                get_static_hs_lib_name(ctx.attr.version, lib): {"static": lib}
                for lib in target[HaskellImportHack].static_profiling_libraries.to_list()
            }
        else:
            # Workaround for https://github.com/tweag/rules_haskell/issues/881
            # Static and dynamic libraries don't necessarily pair up 1 to 1.
            # E.g. the rts package in the Unix GHC bindist contains the
            # dynamic libHSrts and the static libCffi and libHSrts.
            libs = {}
            for lib in target[HaskellImportHack].dynamic_libraries.to_list():
                libname = get_dynamic_hs_lib_name(ctx.attr.version, lib)
                if libname == "ffi" and libname in libs:
                    # Make sure that the file of libffi matching its soname
                    # ends up in target runfiles. Otherwise, execution will
                    # fail with "cannot open shared object file" errors.
                    # On Linux libffi comes in three shapes:
                    #   libffi.so, libffi.so.7, libffi.so.7.1.0
                    # (version numbers may vary)
                    # The soname is then libffi.so.7, meaning, at runtime the
                    # dynamic linker will look for libffi.so.7. So, that file
                    # should be the LibraryToLink.dynamic_library.
                    ext_components = get_lib_extension(lib).split(".")
                    if len(ext_components) == 2 and ext_components[0] == "so":
                        libs[libname]["dynamic"] = lib
                else:
                    # with GHC >= 9.4.1 the rts library has a version number
                    # included in the name.
                    # for handling single-threaded and threading variants below,
                    # we normalize the name and strip the version number
                    if libname.startswith("HSrts-"):
                        idx = libname.find("_")
                        suffix = libname[idx:] if idx > 0 else ""
                        libname = "HSrts" + suffix

                    libs[libname] = {"dynamic": lib}
            for lib in target[HaskellImportHack].static_libraries.to_list():
                name = get_static_hs_lib_name(with_profiling, lib)

                # with GHC >= 9.4.1 the rts library has a version number
                # included in the name.
                # for handling single-threaded and threading variants below,
                # we normalize the name and strip the version number
                if name.startswith("HSrts-"):
                    idx = name.find("_")
                    suffix = name[idx:] if idx > 0 else ""
                    name = "HSrts" + suffix

                entry = libs.get(name, {})
                entry["static"] = lib
                libs[name] = entry

            # Avoid duplicate runtime and ffi libraries. These libraries come
            # in threaded and non-threaded flavors. Depending on the
            # compilation mode we want to forward only one or the other.
            # XXX: Threaded mode should be a per-target property. Use Bazel
            # build configurations and transitions to select the threaded or
            # non-threaded runtime and ffi on a per-target basis.
            if "HSrts_thr" in libs:
                if with_threaded:
                    libs["HSrts"] = libs["HSrts_thr"]
                libs.pop("HSrts_thr")
            if "Cffi_thr" in libs:
                if with_threaded:
                    libs["ffi"]["static"] = libs["Cffi_thr"]["static"]
                libs.pop("Cffi_thr")
        linker_inputs = [
            cc_common.create_linker_input(
                owner = ctx.label,
                libraries = depset(direct = [
                    cc_common.create_library_to_link(
                        actions = ctx.actions,
                        feature_configuration = feature_configuration,
                        dynamic_library = lib.get("dynamic", None),
                        dynamic_library_symlink_path =
                            _toolchain_library_symlink(lib["dynamic"]) if lib.get("dynamic") else "",
                        static_library = lib.get("static", None),
                        cc_toolchain = cc_toolchain,
                    )
                    for lib in libs.values()
                ]),
                user_link_flags = depset(direct = target[HaskellImportHack].linkopts),
            ),
        ]
        compilation_context = cc_common.create_compilation_context(
            headers = target[HaskellImportHack].headers,
            includes = target[HaskellImportHack].includes,
        )
        linking_context = cc_common.create_linking_context(
            linker_inputs = depset(direct = linker_inputs),
        )
        cc_info = CcInfo(
            compilation_context = compilation_context,
            linking_context = linking_context,
        )
        library_dict[package] = struct(
            default_info = target[DefaultInfo],
            hs_info = target[HaskellInfo],
            hs_lib_info = target[HaskellLibraryInfo],
            cc_info = cc_common.merge_cc_infos(cc_infos = [cc_info] + [
                library_dict[dep].cc_info
                for dep in target[HaskellImportHack].depends
            ]),
            haddock_info = target[HaddockInfo],
        )

    return HaskellToolchainLibraries(libraries = library_dict)

def _haskell_toolchain_impl(ctx):
    numeric_version = [int(x) for x in ctx.attr.version.split(".")]
    if numeric_version == [8, 10, 1] or numeric_version == [8, 10, 2]:
        fail("GHC 8.10.1 and 8.10.2 not supported. Upgrade to 8.10.3 or later.")

    if ctx.attr.asterius_binaries:
        # we recover binaries that are not provided by asterius via the regular toolchain.
        exec_tools_struct = ctx.attr._exec_haskell_toolchain[platform_common.ToolchainInfo].tools

        tools_struct_args = {
            k: getattr(exec_tools_struct, k)
            for name in _GHC_BINARIES
            for k in [name.replace("-", "_")]
        }
        ahc_binaries = _lookup_binaries(ASTERIUS_BINARIES.keys(), ctx.files.asterius_binaries)
        for tool, asterius_binary in ahc_binaries.items():
            tools_struct_args[ASTERIUS_BINARIES[tool]] = asterius_binary
    else:
        ghc_tools = _GHC_BINARIES

        # GHC > 9.10 does not install ghci with relocatable = true, add the tool if it is available
        if any([file.basename.startswith("ghci") for file in ctx.files.tools]):
            ghc_tools = ghc_tools + ["ghci"]
        else:
            print(
                "WARN: ghci binary is not available for {}, `tools.ghci` will not exist on its haskell toolchain".format(
                    ctx.label.repo_name,
                ),
            )

        ghc_binaries = _lookup_binaries(ghc_tools, ctx.files.tools, ctx.attr.version)
        tools_struct_args = {
            name.replace("-", "_"): file
            for name, file in ghc_binaries.items()
        }

    # Get the libdir and docdir paths
    libdir = ctx.files.libdir
    if ctx.attr.libdir_path:
        libdir_path = ctx.attr.libdir_path
    elif libdir:
        # Find the `lib/settings` file and infer `libdir` from its path.
        for f in libdir:
            if f.path.endswith("lib/settings"):
                libdir_path = paths.dirname(f.path)
                break
        if libdir_path == None:
            fail("Could not infer `libdir_path` from provided `libdir` attribute. Missing `lib/settings` file.", "libdir")
    else:
        fail("One of `libdir` and `libdir_path` is required.")

    docdir = ctx.files.docdir
    if ctx.attr.docdir_path:
        docdir_path = ctx.attr.docdir_path
    elif docdir:
        docdir_path = None

        # Find a file matching `html/libraries/base-*.*.*.*/*` and infer `docdir` from its path.
        # `GHC.Paths.docdir` reports paths such as `.../doc/html/libraries/base-4.13.0.0`.
        for f in docdir:
            html_start = f.path.find("html/libraries/base")
            if html_start != -1:
                base_end = f.path.find("/", html_start + len("html/libraries/base"))
                if base_end != -1:
                    docdir_path = f.path[:base_end]
                    break
        if docdir_path == None:
            fail("Could not infer `docdir_path` from provided `docdir` attribute. Missing `lib/settings` file.", "docdir")
    else:
        fail("One of `docdir` and `docdir_path` is required.")

    # Get the versions of every prebuilt package.
    ghc_pkg = tools_struct_args["ghc_pkg"]
    pkgdb_file = ctx.actions.declare_file("ghc-global-pkgdb")
    ctx.actions.run_shell(
        inputs = [ghc_pkg],
        outputs = [pkgdb_file],
        tools = ctx.files.tools if ctx.attr.asterius_binaries else [],
        mnemonic = "HaskellPackageDatabaseDump",
        command = "{ghc_pkg} dump --global > {output}".format(
            ghc_pkg = ghc_pkg.path,
            output = pkgdb_file.path,
        ),
    )

    locale_archive = None

    if ctx.attr.locale_archive != None:
        locale_archive = ctx.file.locale_archive

    libraries = {
        lib.label.name: lib
        for lib in ctx.attr.libraries
    }

    (cc_wrapper_inputs, cc_wrapper_manifest) = ctx.resolve_tools(tools = [ctx.attr._cc_wrapper])
    cc_wrapper_info = ctx.attr._cc_wrapper[DefaultInfo]
    cc_wrapper_runfiles = cc_wrapper_info.default_runfiles.merge(
        cc_wrapper_info.data_runfiles,
    )

    if ctx.attr.asterius_binaries:
        tools_config = asterius_tools_config(
            exec_cc_toolchain = ctx.attr._exec_cc_toolchain[cc_common.CcToolchainInfo],
            posix_toolchain = ctx.attr._exec_posix_toolchain[platform_common.ToolchainInfo],
            node_toolchain = ctx.attr._exec_nodejs_toolchain[platform_common.ToolchainInfo],
            tools_for_ghc_pkg = ctx.files.tools,
        )
    else:
        tools_config = struct(
            path_for_run_ghc = default_tools_config.path_for_run_ghc + ctx.attr._exec_posix_toolchain[platform_common.ToolchainInfo].paths,
            tools_for_ghc = default_tools_config.tools_for_ghc,
            path_for_cabal = default_tools_config.path_for_cabal,
            tools_for_ghc_pkg = default_tools_config.tools_for_ghc_pkg,
            maybe_exec_cc_toolchain = default_tools_config.maybe_exec_cc_toolchain,
            supports_haddock = default_tools_config.supports_haddock,
        )

    (protoc_inputs, protoc_input_manifests) = ctx.resolve_tools(tools = [ctx.attr._protoc])

    return [
        platform_common.ToolchainInfo(
            name = ctx.label.name,
            tools = struct(**tools_struct_args),
            bindir = ctx.files.tools,
            libdir = libdir,
            libdir_path = libdir_path,
            docdir = docdir,
            docdir_path = docdir_path,
            ghcopts = ctx.attr.ghcopts,
            repl_ghci_args = ctx.attr.repl_ghci_args,
            haddock_flags = ctx.attr.haddock_flags,
            cabalopts = ctx.attr.cabalopts,
            locale = ctx.attr.locale,
            locale_archive = locale_archive,
            cc_wrapper = struct(
                executable = ctx.executable._cc_wrapper,
                inputs = cc_wrapper_inputs,
                manifests = cc_wrapper_manifest,
                runfiles = cc_wrapper_runfiles,
            ),
            mode = ctx.var["COMPILATION_MODE"],
            actions = struct(
                compile_binary = compile_binary,
                compile_library = compile_library,
                link_binary = link_binary,
                link_library_dynamic = link_library_dynamic,
                link_library_static = link_library_static,
                package = package,
                run_ghc = _run_ghc,
            ),
            old_libraries = libraries,
            new_libraries = _haskell_toolchain_libraries(ctx, libraries),
            is_darwin = ctx.attr.is_darwin,
            is_windows = ctx.attr.is_windows,
            static_runtime = ctx.attr.static_runtime,
            fully_static_link = ctx.attr.fully_static_link,
            version = ctx.attr.version,
            numeric_version = numeric_version,
            global_pkg_db = pkgdb_file,
            protoc = struct(
                executable = ctx.executable._protoc,
                inputs = protoc_inputs,
                input_manifests = protoc_input_manifests,
            ),
            rule_info_proto = ctx.attr._rule_info_proto,
            tools_config = tools_config,
        ),
    ]

common_attrs = {
    "tools": attr.label_list(
        mandatory = True,
    ),
    "libraries": attr.label_list(
        mandatory = True,
    ),
    "libdir": attr.label_list(
        doc = "The files contained in GHC's libdir that Bazel should track. C.f. `ghc --print-libdir`. Do not specify this for a globally installed GHC distribution, e.g. a Nix provided one. One of `libdir` or `libdir_path` is required.",
    ),
    "libdir_path": attr.string(
        doc = "The absolute path to GHC's libdir. C.f. `ghc --print-libdir`. Specify this if `libdir` is left empty. One of `libdir` or `libdir_path` is required.",
    ),
    "docdir": attr.label_list(
        doc = "The files contained in GHC's docdir that Bazel should track. C.f. `GHC.Paths.docdir` from `ghc-paths`. Do not specify this for a globally installed GHC distribution, e.g. a Nix provided one. One of `docdir` or `docdir_path` is required.",
    ),
    "docdir_path": attr.string(
        doc = "The absolute path to GHC's docdir. C.f. `GHC.Paths.docdir` from `ghc-paths`. Specify this if `docdir` is left empty. One of `docdir` or `docdir_path` is required.",
    ),
    "ghcopts": attr.string_list(),
    "repl_ghci_args": attr.string_list(),
    "haddock_flags": attr.string_list(),
    "cabalopts": attr.string_list(),
    "version": attr.string(
        mandatory = True,
    ),
    "is_darwin": attr.bool(
        doc = "Whether compile on and for Darwin (macOS).",
        mandatory = True,
    ),
    "is_windows": attr.bool(
        doc = "Whether compile on and for Windows.",
        mandatory = True,
    ),
    "static_runtime": attr.bool(),
    "fully_static_link": attr.bool(),
    "locale": attr.string(
        default = "C.UTF-8",
        doc = "Locale that will be set during compiler invocations.",
    ),
    "locale_archive": attr.label(
        allow_single_file = True,
    ),
    "asterius_binaries": attr.label(),
    "_cc_wrapper": attr.label(
        cfg = "exec",
        default = Label("@rules_haskell//haskell:cc_wrapper"),
        executable = True,
    ),
    "_protoc": attr.label(
        executable = True,
        cfg = "exec",
        # N.B. can be overridden by `--proto_compiler` flag
        default = configuration_field("proto", "proto_compiler"),
    ),
    "_rule_info_proto": attr.label(
        allow_single_file = True,
        default = Label("@rules_haskell//rule_info:rule_info_proto"),
    ),
    "_exec_posix_toolchain": attr.label(
        default = Label("@rules_haskell//haskell:current_posix_toolchain"),
        cfg = "exec",
    ),
}

_ahc_haskell_toolchain = rule(
    _haskell_toolchain_impl,
    attrs = dict(
        common_attrs,
        _cc_toolchain = attr.label(
            default = Label(
                "@rules_cc//cc:current_cc_toolchain",
            ),
        ),
        _exec_nodejs_toolchain = attr.label(
            default = Label("@rules_haskell//haskell:current_nodejs_toolchain"),
            cfg = "exec",
        ),
        _exec_haskell_toolchain = attr.label(
            default = Label("@rules_haskell//haskell:current_haskell_toolchain"),
            cfg = "exec",
        ),
        _exec_cc_toolchain = attr.label(
            default = Label("@rules_haskell//haskell:current_cc_toolchain"),
            cfg = "exec",
        ),
    ),
    toolchains = use_cc_toolchain(mandatory = True),
    fragments = ["cpp"],
)

_haskell_toolchain = rule(
    _haskell_toolchain_impl,
    attrs = dict(
        common_attrs,
        _cc_toolchain = attr.label(
            default = Label(
                "@rules_cc//cc:current_cc_toolchain",
            ),
        ),
    ),
    toolchains = use_cc_toolchain(mandatory = True),
    fragments = ["cpp"],
)

def haskell_toolchain(
        name,
        version,
        static_runtime,
        fully_static_link,
        tools,
        libraries,
        asterius_binaries = None,
        ghcopts = [],
        repl_ghci_args = [],
        haddock_flags = [],
        cabalopts = [],
        locale_archive = None,
        **kwargs):
    """Declare a compiler toolchain.

    You need at least one of these declared somewhere in your `BUILD` files
    for the other rules to work. Once declared, you then need to *register*
    the toolchain using `register_toolchains` in your `WORKSPACE` file (see
    example below).

    ### Examples

      In a `BUILD` file:

      ```bzl
      haskell_toolchain(
          name = "ghc",
          version = "1.2.3",
          static_runtime = static_runtime,
          fully_static_link = fully_static_link,
          tools = ["@sys_ghc//:bin"],
          ghcopts = ["-Wall"],
      )
      ```

      where `@sys_ghc` is an external repository defined in the `WORKSPACE`,
      e.g. using:

      ```bzl
      nixpkgs_package(
          name = 'sys_ghc',
          attribute_path = 'haskell.compiler.ghc822',
      )

      register_toolchains("//:ghc")
      ```

    Args:
      name: A unique name for this toolchain.
      version: Version of your GHC compiler. It has to match the version reported by the GHC used by bazel.
      static_runtime: Whether GHC was linked with a static runtime.
      fully_static_link: Whether GHC should build fully-statically-linked binaries.
      tools: GHC and executables that come with it. First item takes precedence.
      libraries: The set of libraries that come with GHC. Requires haskell_import targets.
      asterius_binaries: An optional filegroup containing asterius binaries.
        If present the toolchain will target WebAssembly and only use binaries from `tools` if needed to complete the toolchain.
      ghcopts: A collection of flags that will be passed to GHC on every invocation.
      repl_ghci_args: A collection of flags that will be passed to GHCI on repl invocation. It extends the `ghcopts` collection.\\
        Flags set here have precedance over `ghcopts`.
      haddock_flags: A collection of flags that will be passed to haddock.
      cabalopts: Additional flags to pass to `Setup.hs configure` for all Cabal rules.\\
        Note, Cabal rules do not read the toolchain attributes `ghcopts`, `compiler_flags` or `haddock_flags`.\\
        Use `--ghc-option=OPT` to configure additional compiler flags.\\
        Use `--haddock-option=OPT` to configure additional haddock flags.\\
        Use `--haddock-option=--optghc=OPT` if haddock generation requires additional compiler flags.
      locale_archive: Label pointing to the locale archive file to use.\\
        Linux-specific and mostly useful on NixOS.
      **kwargs: Common rule attributes. See [Bazel documentation](https://docs.bazel.build/versions/master/be/common-definitions.html#common-attributes).

    """
    if "compiler_flags" in kwargs:
        fail("`compiler_flags` argument was removed, use `ghcopts` instead")

    corrected_ghci_args = repl_ghci_args + ["-no-user-package-db"]

    toolchain_rule = _ahc_haskell_toolchain if asterius_binaries else _haskell_toolchain
    toolchain_rule(
        name = name,
        version = version,
        static_runtime = static_runtime,
        fully_static_link = fully_static_link,
        tools = tools,
        libraries = libraries,
        ghcopts = ghcopts,
        repl_ghci_args = corrected_ghci_args,
        haddock_flags = haddock_flags,
        cabalopts = cabalopts,
        is_darwin = select({
            "@rules_haskell//haskell/platforms:darwin": True,
            "//conditions:default": False,
        }),
        is_windows = select({
            "@rules_haskell//haskell/platforms:mingw32": True,
            "//conditions:default": False,
        }),
        # Ignore this attribute on any platform that is not Linux. The
        # LOCALE_ARCHIVE environment variable is a Linux-specific
        # Nixpkgs hack.
        locale_archive = select({
            "@rules_haskell//haskell/platforms:linux": locale_archive,
            "//conditions:default": None,
        }),
        asterius_binaries = asterius_binaries,
        **kwargs
    )

def rules_haskell_toolchains(
        version = None,
        compiler_flags = None,  # TODO remove
        ghcopts = None,
        haddock_flags = None,
        repl_ghci_args = None,
        cabalopts = None,
        locale = None):
    """Register GHC binary distributions for all platforms as toolchains.

    Toolchains can be used to compile Haskell code. This function
    registers one toolchain for each known binary distribution on all
    platforms of the given GHC version. During the build, one
    toolchain will be selected based on the host and target platforms
    (See [toolchain resolution][toolchain-resolution]).

    [toolchain-resolution]: https://docs.bazel.build/versions/master/toolchains.html#toolchain-resolution

    Args:
      version: The desired GHC version
      locale: Locale that will be set during compiler
        invocations. Default: C.UTF-8 (en_US.UTF-8 on MacOS)
      ghcopts: A collection of flags that will be passed to GHC on every invocation.

    """
    if compiler_flags:
        fail("`compiler_flags` argument was removed, use `ghcopts` instead")

    haskell_register_ghc_bindists(
        version = version,
        ghcopts = ghcopts,
        haddock_flags = haddock_flags,
        repl_ghci_args = repl_ghci_args,
        cabalopts = cabalopts,
        locale = locale,
    )

# Utility rules forwarding various toolchain providers.
# These rules are used in order to depend on toolchains through the attrs attribute,
# and control their configurations using the `cfg` field.

def get_nodejs_toolchain_impl(ctx):
    return ctx.toolchains["@rules_nodejs//nodejs:toolchain_type"]

get_nodejs_toolchain = rule(
    get_nodejs_toolchain_impl,
    toolchains = [
        "@rules_nodejs//nodejs:toolchain_type",
    ],
)

def get_posix_toolchain_impl(ctx):
    return ctx.toolchains["@rules_sh//sh/posix:toolchain_type"]

get_posix_toolchain = rule(
    get_posix_toolchain_impl,
    toolchains = [
        "@rules_sh//sh/posix:toolchain_type",
    ],
)

def get_haskell_toolchain_impl(ctx):
    return ctx.toolchains["@rules_haskell//haskell:toolchain"]

get_haskell_toolchain = rule(
    get_haskell_toolchain_impl,
    toolchains = [
        "@rules_haskell//haskell:toolchain",
    ],
)

def get_cc_toolchain_impl(ctx):
    return find_cc_toolchain(ctx)

get_cc_toolchain = rule(
    get_cc_toolchain_impl,
    toolchains = use_cc_toolchain(),
    attrs = {
        "_cc_toolchain": attr.label(
            default = Label(
                "@rules_cc//cc:current_cc_toolchain",
            ),
        ),
    },
)
