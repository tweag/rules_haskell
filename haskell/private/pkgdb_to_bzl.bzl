load(":private/workspace_utils.bzl", "find_python")

def pkgdb_to_bzl(repository_ctx, paths, libdir):
    """Generate a BUILD file from a package database.

    Also creates symlinks to any resources stored outside of the GHC
    base directory.

    Args:
      repository_ctx: repository context.
      paths: a dictionary of label names to paths.
      pkgdir: the relative location of GHC's libdir
    """
    result = repository_ctx.execute([
        find_python(repository_ctx),
        paths["@rules_haskell//haskell:private/pkgdb_to_bzl.py"],
        repository_ctx.attr.name,
        libdir,
    ])
    if result.return_code:
        fail("Error executing pkgdb_to_bzl.py: {stderr}".format(stderr = result.stderr))

    result_dict = json.decode(result.stdout)

    # Haddock files on nixpkgs are stored outside of the ghc package
    # The pkgdb_to_bzl.py program generates bazel labels for theses files
    # and asks the parent process to generate the associated bazel symlink
    for line in result_dict["file_content"].split("\n"):
        if line.startswith("#SYMLINK:"):
            _, path, name = line.split(" ")
            repository_ctx.symlink(path, name)

    return result_dict
