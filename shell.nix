let
  p = import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/7c3dc2f53fc837be79426f11c9133f73d15a05c4.tar.gz") {};
  bazelTools = import ./rules_nixpkgs.nix { nixpkgs = p; };
in
bazelTools.buildEnv {
  name = "rules_haskell";
  packages = {
    ghc = {
      nixPackage = p.callPackage ./tests/ghc.nix {};
      buildFile = p.writeText "BUILD.ghc" ''
        package(default_visibility = ["//visibility:public"])

        filegroup(
        name = "bin",
        srcs = glob(["bin/*"]),
        )

        cc_library(
        name = "threaded-rts",
        srcs = select({
            "@bazel_tools//src/conditions:darwin":
                glob([
                    "lib/ghc-*/rts/libHSrts_thr-ghc*.dylib",
                    "lib/ghc-*/rts/libffi.dylib",
                ]),
            "//conditions:default":
                glob([
                    "lib/ghc-*/rts/libHSrts_thr-ghc*.so",
                    "lib/ghc-*/rts/libffi.so.6",
                ]),
        }),
        hdrs = glob(["lib/ghc-*/include/**/*.h"]),
        strip_include_prefix = glob(["lib/ghc-*/include"], exclude_directories=0)[0],
        )
      '';
    };
    protoc_gen_haskell = bazelTools.withDefaultBuild (p.callPackage ./tests/protoc_gen_haskell.nix {});
    doctest = bazelTools.withDefaultBuild p.haskell.packages.ghc822.doctest;
    c2hs = bazelTools.withDefaultBuild p.haskell.packages.ghc822.c2hs;
    zlib = {
      nixPackage = p.zlib;
      buildFile = p.writeText "BUILD.zlib" ''
        package(default_visibility = ["//visibility:public"])

        filegroup (
        name = "lib",
        srcs = glob([
            "lib/*.so",
            "lib/*.so.*",
            "lib/*.dylib",
        ]),
        testonly = 1,
        )
      '';
    };
    zlib_dev = {
      nixPackage = p.zlib.dev;
      buildFile = p.writeText "BUILD.zlib.dev" ''
        load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_cc_import")
        package(default_visibility = ["//visibility:public"])

        filegroup (
        name = "include",
        srcs = glob(["include/*.h"]),
        testonly = 1,
        )

        haskell_cc_import(
            name = "zlib",
            shared_library = "@zlib//:lib",
            hdrs = [":include"],
            testonly = 1,
            strip_include_prefix = "include",
        )
        '';
    };
  };
}
