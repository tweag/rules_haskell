"""Rules for defining toolchains"""

load("@bazel_skylib//lib:paths.bzl", "paths")
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
)
load(":private/actions/package.bzl", "package")

_GHC_BINARIES = ["ghc", "ghc-pkg", "hsc2hs", "haddock", "ghci", "runghc", "hpc"]

def _run_ghc(hs, cc, inputs, outputs, mnemonic, arguments, params_file = None, env = None, progress_message = None, input_manifests = None):
    if not env:
        env = hs.env

    args = hs.actions.args()
    args.add(hs.tools.ghc)

    # Do not use Bazel's CC toolchain on Windows, as it leads to linker and librarty compatibility issues.
    # XXX: We should also tether Bazel's CC toolchain to GHC's, so that we can properly mix Bazel-compiled
    # C libraries with Haskell targets.
    if not hs.toolchain.is_windows:
        args.add_all([
            # GHC uses C compiler for assemly, linking and preprocessing as well.
            "-pgma",
            cc.tools.cc,
            "-pgmc",
            cc.tools.cc,
            "-pgml",
            cc.tools.cc,
            "-pgmP",
            cc.tools.cc,
            # Setting -pgm* flags explicitly has the unfortunate side effect
            # of resetting any program flags in the GHC settings file. So we
            # restore them here. See
            # https://ghc.haskell.org/trac/ghc/ticket/7929.
            "-optc-fno-stack-protector",
            "-optP-E",
            "-optP-undef",
            "-optP-traditional",
        ])

    compile_flags_file = hs.actions.declare_file("compile_flags_%s_%s" % (hs.name, mnemonic))
    extra_args_file = hs.actions.declare_file("extra_args_%s_%s" % (hs.name, mnemonic))

    args.set_param_file_format("multiline")
    arguments.set_param_file_format("multiline")
    hs.actions.write(compile_flags_file, args)
    hs.actions.write(extra_args_file, arguments)

    extra_inputs = [
        hs.tools.ghc,
        # Depend on the version file of the Haskell toolchain,
        # to ensure the version comparison check is run first.
        hs.toolchain.version_file,
        compile_flags_file,
        extra_args_file,
    ] + cc.files

    if params_file:
        params_file_src = params_file.path
        extra_inputs.append(params_file)
    else:
        params_file_src = "<(:)"  # a temporary file with no contents

    script = """
export PATH=${PATH:-} # otherwise GCC fails on Windows

# this is equivalent to 'readarray'. We do not use 'readarray' in order to
# support older bash versions.
while IFS= read -r line; do compile_flags+=("$line"); done < %s
while IFS= read -r line; do extra_args+=("$line"); done < %s
while IFS= read -r line; do param_file_args+=("$line"); done < %s

"${compile_flags[@]}" "${extra_args[@]}" ${param_file_args+"${param_file_args[@]}"}
""" % (compile_flags_file.path, extra_args_file.path, params_file_src)

    ghc_wrapper_name = "ghc_wrapper_%s_%s" % (hs.name, mnemonic)
    ghc_wrapper = hs.actions.declare_file(ghc_wrapper_name)
    hs.actions.write(ghc_wrapper, script, is_executable = True)
    extra_inputs.append(ghc_wrapper)

    if type(inputs) == type(depset()):
        inputs = depset(extra_inputs, transitive = [inputs])
    else:
        inputs += extra_inputs

    hs.actions.run_shell(
        inputs = inputs,
        input_manifests = input_manifests,
        outputs = outputs,
        command = ghc_wrapper.path,
        mnemonic = mnemonic,
        progress_message = progress_message,
        env = env,
        arguments = [],
    )

    return args

def _haskell_toolchain_impl(ctx):
    # Store the binaries of interest in ghc_binaries.
    ghc_binaries = {}
    for tool in _GHC_BINARIES:
        for file in ctx.files.tools:
            if tool in ghc_binaries:
                continue

            basename_no_ext = paths.split_extension(file.basename)[0]
            if tool == basename_no_ext:
                ghc_binaries[tool] = file
            elif "%s-%s" % (tool, ctx.attr.version) == basename_no_ext:
                ghc_binaries[tool] = file
        if not tool in ghc_binaries:
            fail("Cannot find {} in {}".format(tool, ctx.attr.tools.label))

    # Run a version check on the compiler.
    version_file = ctx.actions.declare_file("ghc-version")
    ghc = ghc_binaries["ghc"]
    ctx.actions.run_shell(
        inputs = [ghc],
        outputs = [version_file],
        mnemonic = "HaskellVersionCheck",
        command = """
{ghc} --numeric-version > {version_file}
if [[ "{expected_version}" != "$(< {version_file})" ]]
then
    echo ERROR: GHC version does not match expected version.
    echo Your haskell_toolchain specifies {expected_version},
    echo but you have $(< {version_file}) in your environment.
exit 1
fi
        """.format(
            ghc = ghc.path,
            version_file = version_file.path,
            expected_version = ctx.attr.version,
        ),
    )

    # Get the versions of every prebuilt package.
    ghc_pkg = ghc_binaries["ghc-pkg"]
    pkgdb_file = ctx.actions.declare_file("ghc-global-pkgdb")
    ctx.actions.run_shell(
        inputs = [ghc_pkg],
        outputs = [pkgdb_file],
        mnemonic = "HaskellPackageDatabaseDump",
        command = "{ghc_pkg} dump --global > {output}".format(
            ghc_pkg = ghc_pkg.path,
            output = pkgdb_file.path,
        ),
    )

    tools_struct_args = {
        name.replace("-", "_"): file
        for name, file in ghc_binaries.items()
    }

    locale_archive = None

    if ctx.attr.locale_archive != None:
        locale_archive = ctx.file.locale_archive

    libraries = {
        lib.label.name: lib
        for lib in ctx.attr.libraries
    }

    return [
        platform_common.ToolchainInfo(
            name = ctx.label.name,
            tools = struct(**tools_struct_args),
            compiler_flags = ctx.attr.compiler_flags,
            repl_ghci_args = ctx.attr.repl_ghci_args,
            haddock_flags = ctx.attr.haddock_flags,
            locale = ctx.attr.locale,
            locale_archive = locale_archive,
            osx_cc_wrapper_tpl = ctx.file._osx_cc_wrapper_tpl,
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
            libraries = libraries,
            is_darwin = ctx.attr.is_darwin,
            is_windows = ctx.attr.is_windows,
            is_static = ctx.attr.is_static,
            version = ctx.attr.version,
            # Pass through the version_file, that it can be required as
            # input in _run_ghc, to make every call to GHC depend on a
            # successful version check.
            version_file = version_file,
            global_pkg_db = pkgdb_file,
        ),
    ]

_haskell_toolchain = rule(
    _haskell_toolchain_impl,
    attrs = {
        "tools": attr.label_list(
            doc = "GHC and executables that come with it. First item take precedance.",
            mandatory = True,
        ),
        "libraries": attr.label_list(
            doc = "The set of libraries that come with GHC.",
            mandatory = True,
        ),
        "compiler_flags": attr.string_list(
            doc = "A collection of flags that will be passed to GHC on every invocation.",
        ),
        "repl_ghci_args": attr.string_list(
            doc = "A collection of flags that will be passed to GHCI on repl invocation. It extends the `compiler_flags` collection. Flags set here have precedance over `compiler_flags`.",
        ),
        "haddock_flags": attr.string_list(
            doc = "A collection of flags that will be passed to haddock.",
        ),
        "version": attr.string(
            doc = "Version of your GHC compiler. It has to match the version reported by the GHC used by bazel.",
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
        "is_static": attr.bool(
            doc = "Whether GHC was linked statically.",
        ),
        "locale": attr.string(
            default = "en_US.UTF-8",
            doc = "Locale that will be set during compiler invocations.",
        ),
        "locale_archive": attr.label(
            allow_single_file = True,
            doc = """
Label pointing to the locale archive file to use. Mostly useful on NixOS.
""",
        ),
        "_osx_cc_wrapper_tpl": attr.label(
            allow_single_file = True,
            default = Label("@io_tweag_rules_haskell//haskell:private/osx_cc_wrapper.sh.tpl"),
        ),
    },
)

def haskell_toolchain(
        name,
        version,
        tools,
        libraries,
        compiler_flags = [],
        repl_ghci_args = [],
        haddock_flags = [],
        locale_archive = None,
        **kwargs):
    """Declare a compiler toolchain.

    You need at least one of these declared somewhere in your `BUILD` files
    for the other rules to work. Once declared, you then need to *register*
    the toolchain using `register_toolchains` in your `WORKSPACE` file (see
    example below).

    Example:

      In a `BUILD` file:

      ```bzl
      haskell_toolchain(
          name = "ghc",
          version = "1.2.3",
          tools = ["@sys_ghc//:bin"],
          compiler_flags = ["-Wall"],
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
    """
    corrected_ghci_args = repl_ghci_args + ["-no-user-package-db"]
    _haskell_toolchain(
        name = name,
        version = version,
        tools = tools,
        libraries = libraries,
        compiler_flags = compiler_flags,
        repl_ghci_args = corrected_ghci_args,
        haddock_flags = haddock_flags,
        is_darwin = select({
            "@io_tweag_rules_haskell//haskell/platforms:darwin": True,
            "//conditions:default": False,
        }),
        is_windows = select({
            "@io_tweag_rules_haskell//haskell/platforms:mingw32": True,
            "//conditions:default": False,
        }),
        # Ignore this attribute on any platform that is not Linux. The
        # LOCALE_ARCHIVE environment variable is a Linux-specific
        # Nixpkgs hack.
        locale_archive = select({
            "@io_tweag_rules_haskell//haskell/platforms:linux": locale_archive,
            "//conditions:default": None,
        }),
        **kwargs
    )

def haskell_register_toolchains(version):
    """Download the binary distribution of GHC for your current platform
    and register it as a toolchain. This currently has the same effect
    as just `haskell_register_ghc_bindists(version)`.
    """
    haskell_register_ghc_bindists(version)
