/**
  Generate a bazel-friendly nix package containing
  - The haskell package itself
  - Its documentation
  - A bazel file ready to be loaded from the `BUILD` file and containing the
  right call to `haskell_import`
*/
{ runCommand, lib, writeTextDir, symlinkJoin }:
let
  /* Generate the BUILD file for the package */
  genBuildFile =
    { package_name, package, ghc }:
    runCommand "${package_name}-BUILD" {
      preferLocalBuild = true;
      allowSubstitutes = false;
      ghc_pkg = "${ghc}/bin/ghc-pkg --simple-output -v0";
      GHC_PACKAGE_PATH = "${package}/lib/${ghc.name}/package.conf.d";
      inherit package_name;
    } ''
      query_field () {
        $ghc_pkg field ${package_name} "$@"
      }

      query_haddock () {
        echo -n '['
        for FIELD in $(query_field "$@"); do
          echo -n "\"$(echo "$FIELD" | cut -d/ -f5-)*\","
          echo -n "\"$(echo "$FIELD" | cut -d/ -f5-)/*\","
        done
        echo -n ']'
      }

      query_list () {
        echo -n '['
        for FIELD in $(query_field "$@"); do
          echo -n '"'
          echo -n $(echo "$FIELD" | cut -d/ -f5-)
          echo -n '",'
        done
        echo -n ']'
      }

      get_deps () {
        echo -n '['
        for DEP in $(query_field depends); do
          DEPNAME=$(echo $DEP | sed 's/-[0-9].*//')
          # Because of cabal's "internal libraries", we may have a package
          # apparently depending on itself, so we have to filter out this
          # corner-case (see
          # https://github.com/tweag/rules_haskell/pull/442#discussion_r219859467)
          if [[ -n $DEPNAME && $DEPNAME != $(query_field name) ]]; then
            echo -n "\"@hackage-$DEPNAME\","
          fi
        done
        echo -n ']'
      }

      mkdir -p $out
      cat <<EOF > $out/BUILD.bzl
      load("@io_tweag_rules_haskell//haskell:import.bzl", haskell_import_new = "haskell_import")
      deps_repos = $(get_deps)

      def targets():

          haskell_import_new(
              name = "pkg",
              deps = [ dep + "//:pkg" for dep in deps_repos],
              package_id = "$(query_field id)",
              version = "$(query_field version)",
              package_confs = "//:package_conf",
              haddock_interfaces = "//:interfaces",
              haddock_html = "//:html",
          )
          native.filegroup(
            name = "html",
            srcs = native.glob($(query_haddock haddock-html), exclude_directories=1),
          )
          native.filegroup(
            name = "interfaces",
            srcs = native.glob($(query_haddock haddock-interfaces), exclude_directories=0),
          )
          native.filegroup(
            name = "bin",
            srcs = native.glob(["bin/*"]),
          )
          native.filegroup(
            name = "package_conf",
            srcs = native.glob(["lib*/${ghc.name}/package.conf.d/$(query_field name)*.conf"]),
          )
      EOF
    '';
  genAllBuilds = pkgSet:
    let newSet =
      lib.mapAttrs (package_name: package:
      let
        # Some nix packages are actually `null` because the haskell package is
        # bundled with ghc (so it doesn't have a custom derivation of its own).
        # For these, we simply pass the ghc derivation instead of theirs.
        real_package = if builtins.isNull package then pkgSet.ghc else package;
        buildFile = genBuildFile {
          inherit (pkgSet) ghc;
          inherit package_name;
          package = real_package;
        };
      in
      symlinkJoin {
        name = package_name + "-bazel";
        paths = [ real_package (real_package.doc or null) buildFile ];
      }
      )
      pkgSet;
    in
    newSet // {
      packageNames = writeTextDir
        "all-haskell-packages.bzl"
        ("packages =" + builtins.toJSON (builtins.attrNames newSet));
    };
in
genAllBuilds
