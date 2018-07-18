"""Actions for compiling Haskell source code"""

load(":private/java.bzl", "java_interop_info")
load(":private/path_utils.bzl",
  "declare_compiled",
  "target_unique_name",
  "module_name",
  "module_unique_name",
  "get_external_libs_path",
)
load(":private/pkg_id.bzl", "pkg_id")
load(":private/providers.bzl", "DefaultCompileInfo")
load(":private/set.bzl", "set")
load("@bazel_skylib//:lib.bzl", "dicts", "paths")

def _make_ghc_defs_dump(hs, cpp_defines):
  """Generate a file containing GHC default pre-processor definitions.

  Args:
    hs: Haskell context.
    cpp_defines: Location of cpp_defines pattern file.

  Returns:
    File: The file with GHC definitions.
  """
  raw_filename = "ghc-defs-dump-{0}.hs".format(hs.name)
  dummy_src = hs.actions.declare_file(raw_filename)
  ghc_defs_dump_raw = hs.actions.declare_file(paths.replace_extension(raw_filename, ".hspp"))
  ghc_defs_dump = hs.actions.declare_file(paths.replace_extension(raw_filename, ".h"))

  hs.actions.write(dummy_src, "")
  args = hs.actions.args()
  args.add([
    "-E",
    "-optP-dM",
    "-cpp",
    dummy_src.path,
  ])

  hs.toolchain.actions.run_ghc(
    hs,
    inputs = [dummy_src] + hs.extra_binaries,
    outputs = [ghc_defs_dump_raw],
    mnemonic = "HaskellCppDefines",
    arguments = [args],
    env = hs.env,
  )

  hs.actions.run_shell(
    inputs = [ghc_defs_dump_raw, cpp_defines, hs.tools.grep],
    outputs = [ghc_defs_dump],
    command = """
    grep "^[^#]" {cpp_defines} | while IFS= read -r patt; do
      grep "$patt" {raw} >> {filtered}
    done
    """.format(
      cpp_defines = cpp_defines.path,
      raw = ghc_defs_dump_raw.path,
      filtered = ghc_defs_dump.path,
    ),
    env = hs.env,
  )

  return ghc_defs_dump

def _process_hsc_file(hs, cc, ghc_defs_dump, hsc_file):
  """Process a single hsc file.

  Args:
    hs: Haskell context.
    cc: CcInteropInfo, information about C dependencies.
    ghc_defs_dump: File with GHC definitions.
    hsc_file: hsc file to process.

  Returns:
    (File, string): Haskell source file created by processing hsc_file and
       new import directory containing the produced file.
  """
  args = hs.actions.args()

  # Output a Haskell source file.
  hsc_dir_raw = target_unique_name(hs, "hsc")
  hs_out = declare_compiled(hs, hsc_file, ".hs", directory=hsc_dir_raw)
  args.add([hsc_file.path, "-o", hs_out.path])

  args.add(["-c", hs.tools.cc])
  args.add(["-l", hs.tools.cc])
  args.add(["--cflag=" + f for f in cc.cpp_flags])
  args.add(["--cflag=" + f for f in cc.compiler_flags])
  args.add(["--cflag=" + f for f in cc.include_args])
  args.add(["--lflag=" + f for f in cc.linker_flags])
  args.add("-I{0}".format(ghc_defs_dump.dirname))
  args.add("-i{0}".format(ghc_defs_dump.basename))

  hs.actions.run(
    inputs = depset(transitive = [
      depset(cc.hdrs),
      depset([hs.tools.cc]),
      depset([hsc_file, ghc_defs_dump]),
    ]),
    outputs = [hs_out],
    mnemonic = "HaskellHsc2hs",
    executable = hs.tools.hsc2hs,
    arguments = [args],
    env = hs.env,
  )

  idir = paths.join(
    hs.bin_dir.path,
    hs.label.package,
    hsc_dir_raw,
  )

  return hs_out, idir

def _process_chs_file(hs, cc, ghc_defs_dump, chs_file, chi_files=[]):
  """Process a single chs file.

  Args:
    hs: Haskell context.
    cc: CcInteropInfo, information about C dependencies.
    ghc_defs_dump: File with GHC definitions.
    chs_file: chs file to process.
    chi_files: .chi files that should be available to c2hs.

  Returns:
    (File, File, string): Haskell source file created by processing
       chs_file, .chi file produced by the same file, and new import
       directory containing the generated source file.
  """

  args = hs.actions.args()

  # Output a Haskell source file.
  chs_dir_raw = target_unique_name(hs, "chs")
  hs_out = declare_compiled(hs, chs_file, ".hs", directory=chs_dir_raw)
  chi_out = declare_compiled(hs, chs_file, ".chi", directory=chs_dir_raw)
  args.add([chs_file.path, "-o", hs_out.path])

  args.add(["-C-E"])
  args.add(["--cpp", hs.tools.cc.path])
  args.add(["-C-I{0}".format(ghc_defs_dump.dirname)])
  args.add(["-C-include{0}".format(ghc_defs_dump.basename)])
  args.add(["-C" + x for x in cc.cpp_flags])
  args.add(["-C" + x for x in cc.include_args])

  chi_include_root = paths.join(
    hs.bin_dir.path,
    hs.label.workspace_root,
    hs.label.package,
    chs_dir_raw,
  )
  args.add(["-i" + chi_include_root])

  hs.actions.run(
    inputs = depset(transitive = [
      depset(cc.hdrs),
      depset([hs.tools.cc]),
      depset([chs_file, ghc_defs_dump]),
      depset(chi_files),
    ]),
    outputs = [hs_out, chi_out],
    executable = hs.tools.c2hs,
    mnemonic = "HaskellC2Hs",
    arguments = [args],
    env = hs.env,
  )

  idir = paths.join(
    hs.bin_dir.path,
    hs.label.package,
    chs_dir_raw,
  )

  return hs_out, chi_out, idir

def _output_file_ext(base, dynamic, profiling_enabled):
  """Return extension that output of compilation should have depending on the
  following inputs:

  Args:
    base: usually "o" for object files and "hi" for interface files. Preceding
      dot "." will be preserved in the output.
    dynamic: bool, whether we're compiling dynamic object files.
    profiling_enabled: bool, whether profiling is enabled.

  Returns:
    String, extension of Haskell object file.
  """

  with_dot = False
  ext = ""

  if base[0] == '.':
    with_dot = True
    ext = base[1:]
  else:
    ext = base

  if dynamic:
    ext = "dyn_" + ext
  if profiling_enabled:
    ext = "p_" + ext
  return ("." if with_dot else "") + ext

def _compilation_defaults(hs, cc, java, dep_info, srcs, extra_srcs, cpp_defines, compiler_flags, with_profiling, main_file = None, my_pkg_id = None):
  """Declare default compilation targets and create default compiler arguments.

  Returns:
    DefaultCompileInfo: Populated default compilation settings.
  """

  ghc_args = []

  # GHC expects the CC compiler as the assembler, but segregates the
  # set of flags to pass to it when used as an assembler. So we have
  # to set both -optc and -opta.
  cc_args = [
    "-optc" + f for f in cc.compiler_flags
  ] + [
    "-opta" + f for f in cc.compiler_flags
  ]
  ghc_args += cc_args

  # Declare file directories
  objects_dir_raw = target_unique_name(hs, "objects")
  objects_dir = paths.join(
    hs.bin_dir.path,
    hs.label.workspace_root,
    hs.label.package,
    objects_dir_raw,
  )
  interfaces_dir_raw = target_unique_name(hs, "interfaces")
  interfaces_dir = paths.join(
    hs.bin_dir.path,
    hs.label.workspace_root,
    hs.label.package,
    interfaces_dir_raw,
  )

  # Default compiler flags.
  ghc_args += hs.toolchain.compiler_flags
  ghc_args += compiler_flags
  ghc_args.append("-hide-all-packages")

  # Work around macOS linker limits.  This fix has landed in GHC HEAD, but is
  # not yet in a release; plus, we still want to support older versions of
  # GHC.  For details, see: https://phabricator.haskell.org/D4714
  if hs.toolchain.is_darwin:
    ghc_args += ["-optl-Wl,-dead_strip_dylibs"]

  # Expose all prebuilt dependencies
  for prebuilt_dep in set.to_list(dep_info.direct_prebuilt_deps):
    ghc_args += ["-package", prebuilt_dep]

  # Expose all bazel dependencies
  for package in set.to_list(dep_info.package_ids):
    if package != my_pkg_id:
      ghc_args += ["-package-id", package]

  # Only include package DBs for deps, prebuilt deps should be found
  # auto-magically by GHC.
  for cache in set.to_list(dep_info.package_caches):
    ghc_args += ["-package-db", cache.dirname]

  # We want object and dynamic objects from all inputs.
  object_files = []
  object_dyn_files = []

  # We need to keep interface files we produce so we can import
  # modules cross-package.
  interface_files = []
  header_files = []
  boot_files = []
  source_files = set.empty()
  modules = set.empty()

  # Add import hierarchy root.
  # Note that this is not perfect, since GHC requires hs-boot files
  # to be in the same directory as the corresponding .hs file.  Thus
  # the two must both have the same root; i.e., both plain files,
  # both in bin_dir, or both in genfiles_dir.

  import_dirs = set.from_list([
    hs.src_root,
    paths.join(hs.bin_dir.path, hs.src_root),
    paths.join(hs.genfiles_dir.path, hs.src_root),
  ])

  # Output object files are named after modules, not after input file names.
  # The difference is only visible in the case of Main module because it may
  # be placed in a file with a name different from "Main.hs". In that case
  # still Main.o will be produced.

  ghc_defs_dump = _make_ghc_defs_dump(hs, cpp_defines)
  chi_files_so_far = []

  for s in srcs:

    if s.extension == "h":
      header_files.append(s)
    if s.extension in ["hs-boot", "lhs-boot"]:
      boot_files.append(s)
    elif s.extension in ["hs", "lhs", "hsc", "chs"]:
      if not main_file or s != main_file:
        if s.extension == "hsc":
          s0, idir = _process_hsc_file(hs, cc, ghc_defs_dump, s)
          set.mutable_insert(source_files, s0)
          set.mutable_insert(import_dirs, idir)
        elif s.extension == "chs":
          s0, chi, idir = _process_chs_file(hs, cc, ghc_defs_dump, s, chi_files_so_far)
          set.mutable_insert(source_files, s0)
          set.mutable_insert(import_dirs, idir)
          chi_files_so_far.append(chi)
        else:
          set.mutable_insert(source_files, s)
        set.mutable_insert(modules, module_name(hs, s))
        object_files.append(
          declare_compiled(
            hs,
            s,
            _output_file_ext(".o", False, with_profiling),
            directory=objects_dir_raw
          )
        )
        if not with_profiling:
          object_dyn_files.append(
            declare_compiled(
              hs,
              s,
              _output_file_ext(".o", True, with_profiling),
              directory=objects_dir_raw
            )
          )
        interface_files.append(
          declare_compiled(
            hs,
            s,
            _output_file_ext(".hi", False, with_profiling),
            directory=interfaces_dir_raw
          )
        )
        if not with_profiling:
          interface_files.append(
            declare_compiled(
              hs,
              s,
              _output_file_ext(".hi", True, with_profiling),
              directory=interfaces_dir_raw
            )
          )
      else:
        if s.extension == "hsc":
          s0 = _process_hsc_file(hs, cc, ghc_defs_dump, s)
          set.mutable_insert(source_files, s0)
        elif s.extension == "chs":
          s0, chi = _process_chs_file(hs, cc, ghc_defs_dump, s, chi_files_so_far)
          set.mutable_insert(source_files, s0)
          chi_files_so_far.append(chi)
        else:
          set.mutable_insert(source_files, s)
        set.mutable_insert(modules, "Main")
        object_files.append(
          hs.actions.declare_file(
            paths.join(
              objects_dir_raw,
              paths.replace_extension(
                "Main",
                _output_file_ext(".o", False, with_profiling)
              )
            )
          )
        )
        if not with_profiling:
          object_dyn_files.append(
            hs.actions.declare_file(
              paths.join(
                objects_dir_raw,
                paths.replace_extension(
                  "Main",
                  _output_file_ext(".o", True, with_profiling),
                )
              )
            )
          )
        interface_files.append(
          hs.actions.declare_file(
            paths.join(
              interfaces_dir_raw,
              paths.replace_extension(
                "Main",
                _output_file_ext(".hi", False, with_profiling),
              )
            )
          )
        )
        if not with_profiling:
          interface_files.append(
            hs.actions.declare_file(
              paths.join(
                interfaces_dir_raw,
                paths.replace_extension(
                  "Main",
                  _output_file_ext(".hi", True, with_profiling),
                )
              )
            )
          )

  ghc_args += ["-i{0}".format(d) for d in set.to_list(import_dirs)]
  ghc_args += ["-optP" + f for f in cc.cpp_flags]
  ghc_args += cc.include_args

  locale_archive_depset = (
    depset([hs.toolchain.locale_archive])
    if hs.toolchain.locale_archive != None else depset()
  )

  # This is absolutely required otherwise GHC doesn't know what package it's
  # creating `Name`s for to put them in Haddock interface files which then
  # results in Haddock not being able to find names for linking in
  # environment after reading its interface file later.
  if my_pkg_id != None:
    unit_id_args = [
      "-this-unit-id", pkg_id.to_string(my_pkg_id),
      "-optP-DCURRENT_PACKAGE_KEY=\"{}\"".format(pkg_id.to_string(my_pkg_id))
    ]
    ghc_args += unit_id_args

  args = hs.actions.args()
  args.add(ghc_args)

  # Compilation mode and explicit user flags
  if hs.mode == "opt":
    args.add("-O2")

  args.add(["-static"])

  # NOTE We can't have profiling and dynamic code at the same time, see:
  # https://ghc.haskell.org/trac/ghc/ticket/15394
  if with_profiling:
    args.add("-prof")
  else:
    args.add(["-dynamic-too"])

  # Common flags
  args.add([
    "-v0",
    "-c",
    "--make",
    "-fPIC",
    "-hide-all-packages",
  ])

  # Output directories
  args.add([
    "-odir", objects_dir,
    "-hidir", interfaces_dir,
  ])

  # Output file extensions
  args.add([
    "-osuf", _output_file_ext("o", False, with_profiling),
    "-dynosuf", _output_file_ext("o", True, with_profiling),
    "-hisuf", _output_file_ext("hi", False, with_profiling),
    "-dynhisuf", _output_file_ext("hi", True, with_profiling),
  ])

  # Pass source files
  for f in set.to_list(source_files):
    args.add(f)

  return DefaultCompileInfo(
    args = args,
    ghc_args = ghc_args,
    inputs = depset(transitive = [
      depset(header_files),
      depset(boot_files),
      set.to_depset(source_files),
      extra_srcs,
      depset(cc.hdrs),
      set.to_depset(dep_info.package_confs),
      set.to_depset(dep_info.package_caches),
      set.to_depset(dep_info.interface_files),
      depset(dep_info.static_libraries),
      depset(dep_info.static_libraries_prof),
      set.to_depset(dep_info.dynamic_libraries),
      depset(dep_info.external_libraries.values()),
      java.inputs,
      depset([hs.tools.cc]),
      locale_archive_depset,
    ]),
    objects_dir = objects_dir,
    interfaces_dir = interfaces_dir,
    outputs = object_files + object_dyn_files + interface_files,
    object_files = object_files,
    object_dyn_files = object_dyn_files,
    interface_files = interface_files,
    modules = modules,
    header_files = set.from_list(cc.hdrs + header_files),
    boot_files = set.from_list(boot_files),
    source_files = source_files,
    extra_source_files = extra_srcs,
    import_dirs = import_dirs,
    env = dicts.add({
      "LD_LIBRARY_PATH": get_external_libs_path(set.from_list(dep_info.external_libraries.values())),
      },
      java.env,
      hs.env,
    ),
  )

def compile_binary(hs, cc, java, dep_info, srcs, extra_srcs, cpp_defines, compiler_flags, with_profiling, main_file, main_function):
  """Compile a Haskell target into object files suitable for linking.

  Returns:
    struct with the following fields:
      object_files: list of static object files
      object_dyn_files: list of dynamic object files
      modules: set of module names
      source_files: set of Haskell source files
  """
  c = _compilation_defaults(hs, cc, java, dep_info, srcs, extra_srcs, cpp_defines, compiler_flags, with_profiling, main_file = main_file)
  c.args.add(["-main-is", main_function])

  hs.toolchain.actions.run_ghc(
    hs,
    inputs = c.inputs + hs.extra_binaries,
    outputs = c.outputs,
    mnemonic = "HaskellBuildBinary",
    progress_message = "HaskellBuildBinary {}".format(hs.label),
    env = c.env,
    arguments = [c.args]
  )

  return struct(
    object_files = c.object_files,
    object_dyn_files = c.object_dyn_files,
    modules = c.modules,
    source_files = c.source_files,
    import_dirs = c.import_dirs,
    ghc_args = c.ghc_args,
    header_files = c.header_files,
  )

def compile_library(hs, cc, java, dep_info, srcs, extra_srcs, cpp_defines, compiler_flags, with_profiling, my_pkg_id):
  """Build arguments for Haskell package build.

  Returns:
    struct with the following fields:
      interfaces_dir: directory containing interface files
      interface_files: list of interface files
      object_files: list of static object files
      object_dyn_files: list of dynamic object files
      ghc_args: list of string arguments suitable for Haddock
      modules: set of module names
      source_files: set of Haskell module files
      import_dirs: import directories that should make all modules visible (for GHCi)
  """
  c = _compilation_defaults(hs, cc, java, dep_info, srcs, extra_srcs, cpp_defines, compiler_flags, with_profiling, my_pkg_id=my_pkg_id)

  hs.toolchain.actions.run_ghc(
    hs,
    inputs = c.inputs + hs.extra_binaries,
    outputs = c.outputs,
    mnemonic = "HaskellBuildLibrary",
    progress_message = "HaskellBuildLibrary {}".format(hs.label),
    env = c.env,
    arguments = [c.args],
  )

  return struct(
    interfaces_dir = c.interfaces_dir,
    interface_files = c.interface_files,
    object_files = c.object_files,
    object_dyn_files = c.object_dyn_files,
    ghc_args = c.ghc_args,
    modules = c.modules,
    header_files = c.header_files,
    boot_files = c.boot_files,
    source_files = c.source_files,
    extra_source_files = c.extra_source_files,
    import_dirs = c.import_dirs,
  )
