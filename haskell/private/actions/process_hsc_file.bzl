"""Action processing hsc files"""

load("@bazel_skylib//lib:paths.bzl", "paths")
load(":private/version_macros.bzl", "version_macro_includes")
load(":private/path_utils.bzl", "declare_compiled")
load(":private/set.bzl", "set")

def process_hsc_file(hs, cc, hsc_flags, hsc_inputs, hsc_file):
    """Process a single hsc file.

    Args:
      hs: Haskell context.
      cc: CcInteropInfo, information about C dependencies.
      hsc_flags: extra flags to pass to hsc2hs
      hsc_inputs: extra file inputs for the hsc2hs command
      hsc_file: hsc file to process.

    Returns:
      (File, string): Haskell source file created by processing hsc_file and
         new import directory containing the produced file.
    """
    args = hs.actions.args()

    # Output a Haskell source file.
    hsc_dir_raw = paths.join("_hsc", hs.name)
    hs_out = declare_compiled(hs, hsc_file, ".hs", directory = hsc_dir_raw)
    args.add_all([hsc_file.path, "-o", hs_out.path])

    args.add_all(["-c", cc.tools.cc])
    args.add_all(["-l", cc.tools.cc])
    args.add("-ighcplatform.h")
    args.add("-ighcversion.h")
    args.add_all(cc.cpp_flags, format_each = "--cflag=%s")
    args.add_all(cc.compiler_flags, format_each = "--cflag=%s")
    args.add_all(cc.include_args, format_each = "--cflag=%s")
    args.add_all(cc.linker_flags, format_each = "--lflag=%s")

    # If are building fully-statically-linked binaries, we need to ensure that
    # we pass arguments to `hsc2hs` such that objects it builds are statically
    # linked, otherwise we'll get dynamic linking errors when trying to execute
    # those objects to generate code as part of the build.  Since the static
    # configuration should ensure that all the objects involved are themselves
    # statically built, this is just a case of passing `-static` to the linker
    # used by `hsc2hs` (which will be our own wrapper script which eventually
    # calls `gcc`, etc.).
    #
    # Note that we also do this in our Cabal wrapper, where `hsc2hs` might be
    # called by Cabal as part of the build process.
    if hs.toolchain.fully_static_link:
        args.add("--lflag=-static")

    args.add_all(hsc_flags)

    # Add an empty PATH variable if not already specified in hs.env.
    # Needed to avoid a "Couldn't read PATH" error on Windows.
    #
    # On Unix platforms, though, we musn't set PATH as it is automatically set up
    # by the run action, unless already set in the env parameter. This triggers
    # build errors when using GHC bindists on Linux.
    if hs.env.get("PATH") == None and hs.toolchain.is_windows:
        hs.env["PATH"] = ""

    hs.actions.run_shell(
        inputs = depset(transitive = [
            depset(cc.hdrs),
            depset([hsc_file]),
            depset(cc.files),
            depset(hsc_inputs),
            depset(hs.toolchain.bindir),
        ]),
        input_manifests = cc.manifests,
        outputs = [hs_out],
        mnemonic = "HaskellHsc2hs",
        command =
            # cpp (called via c2hs) gets very unhappy if the mingw bin dir is
            # not in PATH so we add it to PATH explicitly.
            """
            export PATH=$PATH:{mingw_bin}

            # Include libdir in include path just like hsc2hs does.
            libdir=$({ghc} --print-libdir)
            # GHC >=9 on Windows stores the includes outside of libdir
            {hsc2hs} -C-I$libdir/include -C-I$libdir/../include "$@"
            """.format(
                mingw_bin = paths.dirname(cc.tools.cc) if hs.toolchain.is_windows else "",
                ghc = hs.tools.ghc.path,
                hsc2hs = hs.tools.hsc2hs.path,
            ),
        arguments = [args],
        env = hs.env,
    )

    idir = paths.join(
        hs.bin_dir.path,
        hs.label.package,
        hsc_dir_raw,
    )

    return hs_out, idir

def preprocess_hsc_flags_and_inputs(dep_info, user_compile_flags, version):
    # Forward all "-D" and "-optP-D" flags to hsc2hs
    hsc_flags = []
    hsc_flags += ["--cflag=" + x for x in user_compile_flags if x.startswith("-D")]
    hsc_flags += ["--cflag=" + x[len("-optP"):] for x in user_compile_flags if x.startswith("-optP-D")]

    hsc_inputs = []
    if version:
        (version_macro_headers, version_macro_flags) = version_macro_includes(dep_info)
        hsc_flags += ["--cflag=" + x for x in version_macro_flags]
        hsc_inputs += set.to_list(version_macro_headers)

    return hsc_flags, hsc_inputs
