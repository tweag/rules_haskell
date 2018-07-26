"""Haddock support"""

load(":private/context.bzl", "haskell_context")
load(":private/path_utils.bzl", "module_name")
load(":private/set.bzl", "set")
load(
    ":private/providers.bzl",
    "HaddockInfo",
    "HaskellBuildInfo",
    "HaskellLibraryInfo",
)
load("@bazel_skylib//:lib.bzl", "paths")

def _get_haddock_path(package_id):
    """Get path to Haddock file of a package given its id.

    Args:
      package_id: string, package id.

    Returns:
      string: relative path to haddock file.
    """
    return package_id + ".haddock"

def _haskell_doc_aspect_impl(target, ctx):
    if HaskellBuildInfo not in target or HaskellLibraryInfo not in target:
        return []

    hs = haskell_context(ctx, ctx.rule.attr)

    package_id = target[HaskellLibraryInfo].package_id
    html_dir_raw = "doc-{0}".format(package_id)
    html_dir = ctx.actions.declare_directory(html_dir_raw)
    haddock_file = ctx.actions.declare_file(_get_haddock_path(package_id))

    args = ctx.actions.args()
    args.add([
        "-D",
        haddock_file.path,
        "--package-name={0}".format(package_id),
        "--package-version={0}".format(target[HaskellLibraryInfo].version),
        "-o",
        html_dir,
        "--html",
        "--hoogle",
        "--title={0}".format(package_id),
        "--hyperlinked-source",
    ])

    transitive_haddocks = {}
    transitive_html = {}

    for dep in ctx.rule.attr.deps:
        if HaddockInfo in dep:
            transitive_haddocks.update(dep[HaddockInfo].transitive_haddocks)
            transitive_html.update(dep[HaddockInfo].transitive_html)

    for pid in transitive_haddocks:
        args.add("--read-interface=../{0},{1}".format(
            pid,
            transitive_haddocks[pid].path,
        ))

    prebuilt_deps = ctx.actions.args()
    for dep in set.to_list(target[HaskellBuildInfo].prebuilt_dependencies):
        prebuilt_deps.add(dep)
    prebuilt_deps.use_param_file(param_file_arg = "%s", use_always = True)

    ghc_args = ctx.actions.args()
    for x in target[HaskellLibraryInfo].ghc_args:
        ghc_args.add(["--optghc", x])
    ghc_args.add([x.path for x in set.to_list(target[HaskellLibraryInfo].source_files)])
    ghc_args.add(["-v0"])

    locale_archive_depset = (
        depset([hs.toolchain.locale_archive]) if hs.toolchain.locale_archive != None else depset()
    )

    ctx.actions.run(
        inputs = depset(transitive = [
            set.to_depset(target[HaskellBuildInfo].package_caches),
            set.to_depset(target[HaskellBuildInfo].interface_files),
            set.to_depset(target[HaskellBuildInfo].dynamic_libraries),
            depset(target[HaskellBuildInfo].external_libraries.values()),
            depset(transitive_haddocks.values()),
            depset(transitive_html.values()),
            # Need to give source files this way because the source_files field of
            # HaskellLibraryInfo provider contains files that are already
            # pre-processed by hsc2hs and these should be visible to Haddock.
            set.to_depset(target[HaskellLibraryInfo].header_files),
            set.to_depset(target[HaskellLibraryInfo].boot_files),
            set.to_depset(target[HaskellLibraryInfo].source_files),
            target[HaskellLibraryInfo].extra_source_files,
            depset([
                hs.tools.bash,
                hs.tools.ghc_pkg,
                hs.tools.haddock,
            ]),
            locale_archive_depset,
        ]),
        outputs = [haddock_file, html_dir],
        mnemonic = "HaskellHaddock",
        progress_message = "HaskellHaddock {}".format(ctx.label),
        executable = ctx.file._haddock_wrapper,
        arguments = [
            prebuilt_deps,
            args,
            ghc_args,
        ],
        env = hs.env,
    )

    transitive_html.update({package_id: html_dir})
    transitive_haddocks.update({package_id: haddock_file})

    return [HaddockInfo(
        package_id = package_id,
        transitive_html = transitive_html,
        transitive_haddocks = transitive_haddocks,
    )]

haskell_doc_aspect = aspect(
    _haskell_doc_aspect_impl,
    attrs = {
        "_haddock_wrapper": attr.label(
            allow_single_file = True,
            default = Label("@io_tweag_rules_haskell//haskell:private/haddock_wrapper.sh"),
        ),
    },
    attr_aspects = ["deps"],
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)

def _haskell_doc_rule_impl(ctx):
    hs = haskell_context(ctx)

    # Reject cases when number of dependencies is 0.

    if not ctx.attr.deps:
        fail("haskell_doc needs at least one haskell_library component in deps")

    doc_root_raw = ctx.attr.name
    haddock_dict = {}
    html_dict_original = {}
    all_caches = set.empty()

    for dep in ctx.attr.deps:
        if HaddockInfo in dep:
            html_dict_original.update(dep[HaddockInfo].transitive_html)
            haddock_dict.update(dep[HaddockInfo].transitive_haddocks)
        if HaskellBuildInfo in dep:
            set.mutable_union(
                all_caches,
                dep[HaskellBuildInfo].package_caches,
            )

    # Copy docs of Bazel deps into predefined locations under the root doc
    # directory.

    html_dict_copied = {}
    doc_root_path = ""

    for package_id in html_dict_original:
        html_dir = html_dict_original[package_id]
        output_dir = ctx.actions.declare_directory(
            paths.join(
                doc_root_raw,
                package_id,
            ),
        )
        doc_root_path = paths.dirname(output_dir.path)

        html_dict_copied[package_id] = output_dir

        ctx.actions.run_shell(
            inputs = [
                hs.tools.cp,
                hs.tools.mkdir,
                html_dir,
            ],
            outputs = [output_dir],
            command = """
      mkdir -p "{doc_dir}"
      # Copy Haddocks of a dependency.
      cp -r "{html_dir}" "{target_dir}"
      """.format(
                doc_dir = doc_root_path,
                html_dir = html_dir.path,
                target_dir = output_dir.path,
            ),
        )

    # Do one more Haddock call to generate the unified index

    index_root_raw = paths.join(doc_root_raw, "index")
    index_root = ctx.actions.declare_directory(index_root_raw)

    args = ctx.actions.args()
    args.add([
        "-o",
        index_root.path,
        "--title={0}".format(ctx.attr.name),
        "--gen-index",
        "--gen-contents",
    ])

    if ctx.attr.index_transitive_deps:
        # Include all packages in the unified index.
        for package_id in html_dict_copied:
            args.add("--read-interface=../{0},{1}".format(
                package_id,
                haddock_dict[package_id].path,
            ))
    else:
        # Include only direct dependencies.
        for dep in ctx.attr.deps:
            if HaddockInfo in dep:
                package_id = dep[HaddockInfo].package_id
                args.add("--read-interface=../{0},{1}".format(
                    package_id,
                    haddock_dict[package_id].path,
                ))

    for cache in set.to_list(all_caches):
        args.add(["--optghc=-package-db={0}".format(cache.dirname)])

    locale_archive_depset = (
        depset([hs.toolchain.locale_archive]) if hs.toolchain.locale_archive != None else depset()
    )

    ctx.actions.run(
        inputs = depset(transitive = [
            set.to_depset(all_caches),
            depset(html_dict_copied.values()),
            depset(haddock_dict.values()),
            locale_archive_depset,
        ]),
        outputs = [index_root],
        mnemonic = "HaskellHaddockIndex",
        executable = hs.tools.haddock,
        arguments = [args],
    )

    return [DefaultInfo(
        files = depset(html_dict_copied.values() + [index_root]),
    )]

haskell_doc = rule(
    _haskell_doc_rule_impl,
    attrs = {
        "deps": attr.label_list(
            aspects = [haskell_doc_aspect],
            doc = "List of Haskell libraries to generate documentation for.",
        ),
        "index_transitive_deps": attr.bool(
            default = False,
            doc = "Whether to include documentation of transitive dependencies in index.",
        ),
    },
    toolchains = ["@io_tweag_rules_haskell//haskell:toolchain"],
)
"""Create API documentation.

Builds API documentation (using [Haddock][haddock]) for the given
Haskell libraries. It will automatically build documentation for any
transitive dependencies to allow for cross-package documentation
linking.

Example:
  ```bzl
  haskell_library(
    name = "my-lib",
    ...
  )

  haskell_doc(
    name = "my-lib-doc",
    deps = [":my-lib"],
  )
  ```

[haddock]: http://haskell-haddock.readthedocs.io/en/latest/
"""
