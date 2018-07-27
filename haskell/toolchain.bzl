"""Rules for defining toolchains"""

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
load("@bazel_skylib//:lib.bzl", "paths")

_GHC_BINARIES = ["ghc", "ghc-pkg", "hsc2hs", "haddock", "ghci"]

def _run_ghc(hs, inputs, outputs, mnemonic, arguments, env = None, progress_message = None):
    if not env:
        env = hs.env

    args = hs.actions.args()
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

    if type(inputs) == type(depset()):
        inputs = depset([hs.tools.cc], transitive = [inputs])
    else:
        inputs += [hs.tools.cc]

    hs.actions.run(
        inputs = inputs,
        outputs = outputs,
        executable = hs.tools.ghc,
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
    for t in ctx.files.tools:
        if t.basename == "ghc":
            compiler = t
    version_file = ctx.actions.declare_file("ghc-version")
    ctx.actions.run_shell(
        inputs = [compiler],
        outputs = [version_file],
        mnemonic = "HaskellVersionCheck",
        command = """
    {compiler} --numeric-version > {version_file}
    if [[ {expected_version} != $(< {version_file}) ]]
    then
        echo GHC version $(< {version_file}) does not match expected version {expected_version}.
        exit 1
    fi
    """.format(
            compiler = compiler.path,
            version_file = version_file.path,
            expected_version = ctx.attr.version,
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
        symlink = ctx.actions.declare_file(
            paths.join(visible_binaries, target),
        )
        symlink_target = targets_r[target]
        if not paths.is_absolute(symlink_target):
            symlink_target = paths.join("/".join([".."] * len(symlink.dirname.split("/"))), symlink_target)
        ctx.actions.run(
            inputs = inputs,
            outputs = [symlink],
            mnemonic = "Symlink",
            executable = "ln",
            # FIXME Currently this part of the process is not hermetic. This
            # should be adjusted when
            #
            # https://github.com/bazelbuild/bazel/issues/4681
            #
            # is implemented.
            use_default_shell_env = True,
            arguments = [
                "-s",
                symlink_target,
                symlink.path,
            ],
        )
        if target == "xcrunwrapper.sh":
            ar_runfiles += [symlink]

        if set.is_member(extra_binaries_names, target):
            extra_binaries_files += [symlink]

        set.mutable_insert(symlinks, symlink)

    targets_w = [
        "bash",
        "cat",
        "cp",
        "grep",
        "ln",
        "mkdir",
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
        ),
        # Make everyone implicitly depend on the version_file, to force
        # the version check.
        DefaultInfo(files = depset([version_file])),
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
        "c2hs": attr.label(
            doc = "c2hs executable",
            allow_single_file = True,
        ),
        "version": attr.string(mandatory = True),
        "is_darwin": attr.bool(mandatory = True),
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
        **kwargs):
    """Declare a compiler toolchain.

    You need at least one of these declared somewhere in your `BUILD` files
    for the other rules to work. Once declared, you then need to *register*
    the toolchain using `register_toolchain` in your `WORKSPACE` file (see
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

      register_toolchain("//:ghc")
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
