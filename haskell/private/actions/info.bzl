"""Defines output groups that are consumed by tools such as 'hrepl'."""

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_skylib//lib:paths.bzl", "paths")
load(":providers.bzl", "all_package_ids")
load(":private/cc_libraries.bzl", "get_ghci_library_files")
load(
    ":private/path_utils.bzl",
    "get_lib_name",
)

def write_proto_file(hs, output_name, proto_type, content):
    """Write an encoded .proto file.

    Writes a file with the text format encoding, and then runs "protoc"
    to convert it to the wire encoding.

    The wire encoding allows us to use released versions of tools
    with different versions of the Haskell rules (within reason).

    Args:
      hs: The current rule context.
      output_name: The output filename.  The text-encoded file will be named
        {output_name}.txt, and the encoded file will be named {output_name}.pb.
      proto_type: The type of the proto (e.g. foo.Bar).  It must be defined
        in rule_info.proto.
      content: The contents of the text file, as a Bazel struct.

    Returns:
      A File containing the encoded proto message, named {file_name}.pb.
    """
    proto_txt = hs.actions.declare_file(output_name + ".txt")
    proto_pb = hs.actions.declare_file(output_name + ".pb")
    hs.actions.write(output = proto_txt, content = content.to_proto())

    protoc = hs.toolchain.protoc
    rule_info_protos = hs.toolchain.rule_info_proto[ProtoInfo].direct_sources

    hs.actions.run_shell(
        outputs = [proto_pb],
        inputs = depset([proto_txt] + rule_info_protos),
        tools = [protoc],
        command =
            "{protoc} {rule_info_proto} --encode {proto_type} < {proto_txt} > {proto_pb}"
                .format(
                protoc = protoc.path,
                proto_type = proto_type,
                proto_txt = proto_txt.path,
                proto_pb = proto_pb.path,
                rule_info_proto = " ".join([p.path for p in rule_info_protos]),
            ),
    )
    return proto_pb

def _filter_package_env(flags):
    # Strips out -package-env from the command-line flags.  Consumers of these output
    # groups will be responsible for setting the right GHC flags themselves,
    # based on the fields of haskell.LibraryInfo.
    result = []
    for i in flags:
        if not flags:
            break
        if flags[0] == "-package-env":
            flags = flags[2:]
        else:
            result.append(flags[0])
            flags = flags[1:]
    return result

def _write_haskell_compile_info(
        workspace_name,
        name,
        hs,
        c,
        cc_libs,
        runfiles):
    return write_proto_file(
        output_name = name,
        hs = hs,
        proto_type = "haskell.CompileInfo",
        content = struct(
            # Calling to_list on c.source_files shouldn't be a performance penalty.
            # Despite being a depset, it only contains sources for the current rule.
            source_files = [f.path for f in c.source_files.to_list()],
            # TODO: currently, this will duplicate the common, target-independent options for
            # each build target.  We should instead move them into GhcConfig.common_options.
            options = _filter_package_env(c.compile_flags),
            transitive_cc_libs = [lib.path for lib in cc_libs],
            # Follows the new runfiles tree organization of:
            # https://github.com/bazelbuild/bazel/wiki/Updating-the-runfiles-tree-structure
            runfiles = [
                struct(
                    full_path = f.path,
                    short_path = paths.join(
                        f.owner.workspace_name or workspace_name,
                        f.short_path,
                    ),
                )
                for f in runfiles.to_list()
            ],
        ),
    )

def library_info_output_groups(
        name,
        hs,
        hs_info,
        lib_info):
    """Output groups for depending on a Haskell target.

    Args:
        name: A string; the name of the current target.
        hs: The Haskell context.
        hs_info: A HaskellInfo provider.
        lib_info: A HaskellLibraryInfo provider.

    Returns:
      A dict whose keys are output groups and values are depsets of Files.
    """
    proto_file = write_proto_file(
        hs = hs,
        output_name = name + ".HaskellLibrary",
        proto_type = "haskell.LibraryInfo",
        content = struct(
            # TODO(google/hrepl#4): currently, we only expose the immediate dependencies.
            transitive_package_ids = [lib_info.package_id],
            transitive_package_dbs =
                [db.dirname for db in hs_info.package_databases.to_list()],
        ),
    )
    return {
        "haskell_transitive_deps": depset(
            transitive = [
                hs_info.package_databases,
                hs_info.interface_dirs,
                hs_info.dynamic_libraries,
            ],
        ),
        "haskell_library_info": depset([proto_file]),
    }

def compile_info_output_groups(
        name,
        workspace_name,
        hs,
        cc,
        c,
        posix,
        runfiles):
    """Output groups for compiling a Haskell target.

    Args:
        name: A string; the name of the current target.
        workspace_name: The workspace this target was defined in.
          Used for organizing its runfiles.
        hs: The Haskell context.
        cc: CcInteropInfo.
        c: A struct with information about the compilation step.
        posix: The posix toolchain.
        runfiles: A depset of Files.

    Returns:
      A dict whose keys are output groups and values are depsets of Files.
    """
    cc_libs = get_ghci_library_files(hs, cc.cc_libraries_info, cc.cc_libraries)
    return {
        "haskell_cdep_libs": depset(cc_libs),
        "haskell_runfiles": runfiles,
        "haskell_source_files": depset(transitive =
                                           [c.source_files, c.extra_source_files]),
        "haskell_compile_info": depset([_write_haskell_compile_info(
            workspace_name = workspace_name,
            hs = hs,
            name = name + ".HaskellCompile",
            c = c,
            cc_libs = cc_libs,
            runfiles = runfiles,
        )]),
    }
