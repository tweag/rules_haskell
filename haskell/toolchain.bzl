"""Rules for defining toolchains"""

load("@bazel_skylib//lib:paths.bzl", "paths")
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
load(":private/set.bzl", "set")

_GHC_BINARIES = ["ghc", "ghc-pkg", "hsc2hs", "haddock", "ghci"]

_get_realpath = """\
function get_realpath() {
    if [ "$(uname -s)" == "Darwin" ]; then
        local queue="$1"
        if [[ "${queue}" != /* ]] ; then
            # Make sure we start with an absolute path.
            queue="${PWD}/${queue}"
        fi
        local current=""
        while [ -n "${queue}" ]; do
            # Removing a trailing /.
            queue="${queue#/}"
            # Pull the first path segment off of queue.
            local segment="${queue%%/*}"
            # If this is the last segment.
            if [[ "${queue}" != */* ]] ; then
                segment="${queue}"
                queue=""
            else
                # Remove that first segment.
                queue="${queue#*/}"
            fi
            local link="${current}/${segment}"
            if [ -h "${link}" ] ; then
                link="$(readlink "${link}")"
                queue="${link}/${queue}"
                if [[ "${link}" == /* ]] ; then
                    current=""
                fi
            else
                current="${link}"
            fi
        done

        echo "${current}"
    else
        readlink -f "$1"
    fi
}
"""

_wrapper_template_relative = """\
set -e

{get_realpath}

DIR="$( cd "$( dirname $(get_realpath "$0") )" >/dev/null && pwd )"

$DIR/{tool_path} "$@"
"""

_wrapper_template_absolute = """\
{tool_path} "$@"
"""

def _run_ghc(hs, inputs, outputs, mnemonic, arguments, params_file = None, env = None, progress_message = None):
    if not env:
        env = hs.env

    args = hs.actions.args()
    args.add([hs.tools.ghc])
    args.add([
        # GHC uses C compiler for assemly, linking and preprocessing as well.
        "-pgma",
        hs.tools.cc.path,
        "-pgmc",
        hs.tools.cc.path,
        "-pgml",
        hs.tools.cc.path,
        "-pgmP",
        hs.tools.cc.path,
        # Setting -pgm* flags explicitly has the unfortunate side effect
        # of resetting any program flags in the GHC settings file. So we
        # restore them here. See
        # https://ghc.haskell.org/trac/ghc/ticket/7929.
        "-optc-fno-stack-protector",
        "-optP-E",
        "-optP-undef",
        "-optP-traditional",
    ])

    extra_inputs = [
        hs.tools.ghc,
        hs.tools.cc,
        # Depend on the version file of the Haskell toolchain,
        # to ensure the version comparison check is run first.
        hs.toolchain.version_file,
    ]
    if params_file:
        command = '${1+"$@"} $(< %s)' % params_file.path
        extra_inputs.append(params_file)
    else:
        command = '${1+"$@"}'

    if type(inputs) == type(depset()):
        inputs = depset(extra_inputs, transitive = [inputs])
    else:
        inputs += extra_inputs

    hs.actions.run_shell(
        inputs = inputs,
        outputs = outputs,
        command = command,
        mnemonic = mnemonic,
        progress_message = progress_message,
        env = env,
        arguments = [args] + arguments,
    )

    return args

def _haskell_toolchain_impl(ctx):
    # Check that we have all that we want.
    for tool in _GHC_BINARIES:
        if tool not in [t.basename for t in ctx.files.tools]:
            fail("Cannot find {} in {}".format(tool, ctx.attr.tools.label))

    # Store the binaries of interest in ghc_binaries.
    ghc_binaries = {}
    for tool in ctx.files.tools:
        if tool.basename in _GHC_BINARIES:
            ghc_binaries[tool.basename] = tool.path

    # Run a version check on the compiler.
    compiler = None
    for t in ctx.files.tools:
        if t.basename == "ghc":
            if compiler:
                fail("There can only be one tool named `ghc` in scope")
            compiler = t
    version_file = ctx.actions.declare_file("ghc-version")
    ctx.actions.run_shell(
        inputs = [compiler],
        outputs = [version_file],
        mnemonic = "HaskellVersionCheck",
        command = """
    {compiler} --numeric-version > {version_file}
    if [[ "{expected_version}" != "$(< {version_file})" ]]
    then
        echo ERROR: GHC version does not match expected version. Your haskell_toolchain specifies {expected_version}, but you have $(< {version_file}) in your environment.
        exit 1
    fi
    """.format(
            compiler = compiler.path,
            version_file = version_file.path,
            expected_version = ctx.attr.version,
        ),
    )

    # Get the versions of every prebuilt package.
    for t in ctx.files.tools:
        if t.basename == "ghc-pkg":
            ghc_pkg = t
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

    # NOTE The only way to let various executables know where other
    # executables are located is often only via the PATH environment variable.
    # For example, ghc uses gcc which is found on PATH. This forces us provide
    # correct PATH value (with e.g. gcc on it) when we call executables such
    # as ghc. However, this hurts hermeticity because if we construct PATH
    # naively, i.e. through concatenation of directory names of all
    # executables of interest, we also make other neighboring executables
    # visible, and they indeed can influence the building process, which is
    # undesirable.
    #
    # To prevent this, we create a special directory populated with symbolic
    # links to all executables we want to make visible, and only to them. Then
    # we make all rules depend on some of these symlinks, and provide PATH
    # (always the same) that points only to this prepared directory with
    # symlinks. This helps hermeticity and forces us to be explicit about all
    # programs we need/use for building. Setting PATH and listing as inputs
    # are both necessary for a tool to be available with this approach.

    visible_binaries = "visible-binaries"
    symlinks = set.empty()
    inputs = []
    inputs.extend(ctx.files.tools)
    inputs.extend(ctx.files._crosstool)

    targets_r = {}
    targets_r.update({
        "ar": ctx.host_fragments.cpp.ar_executable,
        "cc": ctx.host_fragments.cpp.compiler_executable,
        "ld": ctx.host_fragments.cpp.ld_executable,
        "nm": ctx.host_fragments.cpp.nm_executable,
        "cpp": ctx.host_fragments.cpp.preprocessor_executable,
        "strip": ctx.host_fragments.cpp.strip_executable,
    })
    targets_r.update(ghc_binaries)

    # If running on darwin but XCode is not installed (i.e., only the Command
    # Line Tools are available), then Bazel will make ar_executable point to
    # "/usr/bin/libtool". Since we call ar directly, override it.
    # TODO: remove this if Bazel fixes its behavior.
    # Upstream ticket: https://github.com/bazelbuild/bazel/issues/5127.
    if targets_r["ar"].find("libtool") >= 0:
        targets_r["ar"] = "/usr/bin/ar"

    ar_runfiles = []

    # "xcrunwrapper.sh" is a Bazel-generated dependency of the `ar` program on macOS.
    xcrun_candidates = [f for f in ctx.files._crosstool if paths.basename(f.path) == "xcrunwrapper.sh"]
    if xcrun_candidates:
        xcrun = xcrun_candidates[0]
        ar_runfiles += [xcrun]
        targets_r["xcrunwrapper.sh"] = xcrun.path

    if ctx.attr.c2hs != None:
        targets_r["c2hs"] = ctx.file.c2hs.path
        inputs.append(ctx.file.c2hs)

    extra_binaries_names = set.empty()
    for binary in ctx.files.extra_binaries:
        targets_r[binary.basename] = binary.path
        inputs.append(binary)
        set.mutable_insert(extra_binaries_names, binary.basename)

    extra_binaries_files = []

    for target in targets_r:
        wrapper = ctx.actions.declare_file(
            paths.join(visible_binaries, target),
        )
        wrapper_target = targets_r[target]
        if not paths.is_absolute(wrapper_target):
            wrapper_target = paths.join("/".join([".."] * len(wrapper.dirname.split("/"))), wrapper_target)
            wrapper_template = _wrapper_template_relative
        else:
            wrapper_template = _wrapper_template_absolute

        ctx.actions.write(
            wrapper,
            wrapper_template.format(
                tool_path = wrapper_target,
                get_realpath = _get_realpath,
            ),
            is_executable = True,
        )

        if target == "xcrunwrapper.sh":
            ar_runfiles += [wrapper]

        if set.is_member(extra_binaries_names, target):
            extra_binaries_files += [wrapper]

        set.mutable_insert(symlinks, wrapper)

    targets_w = [
        "bash",
        "cat",
        "tr",
        "cp",
        "grep",
        "ln",
        "mkdir",
        "mktemp",
        "rmdir",
    ]

    for target in targets_w:
        symlink = ctx.actions.declare_file(
            paths.join(visible_binaries, paths.basename(target)),
        )
        ctx.actions.run_shell(
            inputs = ctx.files.tools,
            outputs = [symlink],
            mnemonic = "Symlink",
            command = """
      mkdir -p $(dirname "{symlink}")
      ln -s $(which "{target}") "{symlink}"
      """.format(
                target = target,
                symlink = symlink.path,
            ),
            use_default_shell_env = True,
        )
        set.mutable_insert(symlinks, symlink)

    tools_struct_args = {
        tool.basename.replace("-", "_"): tool
        for tool in set.to_list(symlinks)
    }
    tools_runfiles_struct_args = {"ar": ar_runfiles}

    locale_archive = None

    if ctx.attr.locale_archive != None:
        locale_archive = ctx.file.locale_archive

    return [
        platform_common.ToolchainInfo(
            name = ctx.label.name,
            tools = struct(**tools_struct_args),
            tools_runfiles = struct(**tools_runfiles_struct_args),
            extra_binaries = extra_binaries_files,
            compiler_flags = ctx.attr.compiler_flags,
            repl_ghci_args = ctx.attr.repl_ghci_args,
            haddock_flags = ctx.attr.haddock_flags,
            locale = ctx.attr.locale,
            locale_archive = locale_archive,
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
            # All symlinks are guaranteed to be in the same directory so we just
            # provide directory name of the first one (the collection cannot be
            # empty). The rest of the program may rely consider visible_bin_path
            # as the path to visible binaries, without recalculations.
            visible_bin_path = set.to_list(symlinks)[0].dirname,
            is_darwin = ctx.attr.is_darwin,
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
    host_fragments = ["cpp"],
    attrs = {
        "tools": attr.label(
            doc = "GHC and executables that come with it",
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
        "c2hs": attr.label(
            doc = "c2hs executable",
            allow_single_file = True,
        ),
        "version": attr.string(
            doc = "Version of your GHC compiler. It has to match the version reported by the GHC used by bazel.",
            mandatory = True,
        ),
        "is_darwin": attr.bool(
            doc = "Whether compile on and for Darwin (macOS).",
            mandatory = True,
        ),
        # TODO: document
        "_crosstool": attr.label(
            default = Label("//tools/defaults:crosstool"),
        ),
        "extra_binaries": attr.label_list(
            doc = "Additional binaries necessary for building",
            default = [],
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
    },
)

def haskell_toolchain(
        name,
        version,
        tools,
        compiler_flags = [],
        repl_ghci_args = [],
        haddock_flags = [],
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
          version = '1.2.3'
          tools = ["@sys_ghc//:bin"],
          compiler_flags = ["-Wall"],
          c2hs = "@c2hs//:bin", # optional
      )
      ```

      where `@ghc` is an external repository defined in the `WORKSPACE`,
      e.g. using:

      ```bzl
      nixpkgs_package(
          name = 'sys_ghc',
          attribute_path = 'haskell.compiler.ghc822'
      )

      register_toolchains("//:ghc")
      ```

      and for `@c2hs`:

      ```bzl
      nixpkgs_package(
          name = "c2hs",
          attribute_path = "haskell.packages.ghc822.c2hs",
      )
      ```
    """
    impl_name = name + "-impl"
    _haskell_toolchain(
        name = impl_name,
        version = version,
        tools = tools,
        compiler_flags = compiler_flags,
        repl_ghci_args = repl_ghci_args,
        haddock_flags = haddock_flags,
        visibility = ["//visibility:public"],
        is_darwin = select({
            "@bazel_tools//src/conditions:darwin": True,
            "//conditions:default": False,
        }),
        **kwargs
    )

    native.toolchain(
        name = name,
        toolchain_type = "@io_tweag_rules_haskell//haskell:toolchain",
        toolchain = ":" + impl_name,
        exec_compatible_with = [
            "@bazel_tools//platforms:x86_64",
        ],
        target_compatible_with = [
            "@bazel_tools//platforms:x86_64",
        ],
    )
