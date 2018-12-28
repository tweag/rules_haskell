workspace(name = "ai_formation_hazel")

load("@bazel_tools//tools/build_defs/repo:http.bzl",
     "http_archive",
)

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-cd2ed701127ebf7f8f21d37feb1d678e4fdf85e5",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/cd2ed70.tar.gz"],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_git_repository", "nixpkgs_package")

nixpkgs_git_repository(
    name = "nixpkgs",
    revision = "ee80654b5267b07ba10d62d143f211e0be81549e",
)

load("//:cc_configure_custom.bzl", "cc_configure_custom")
nixpkgs_package(
    name = "compiler",
    repository = "@nixpkgs",
    nix_file = "//:compiler.nix",
)

nixpkgs_package(
    name = "binutils",
    repository = "@nixpkgs",
    attribute_path = "binutils"
)

cc_configure_custom(
    name = "local_config_cc",
    gcc = "@compiler//:bin/cc",
    ld = "@binutils//:bin/ld",
)


RULES_HASKELL_SHA = "8bc2b2c847c54f3d9f6bd5000f8deefa1cf4c995"

http_archive(
    name = "io_tweag_rules_haskell",
    urls = ["https://github.com/FormationAI/rules_haskell/archive/"
            + RULES_HASKELL_SHA + ".tar.gz"],
    strip_prefix = "rules_haskell-" + RULES_HASKELL_SHA,
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

nixpkgs_package(
    name = "ghc",
    repository = "@nixpkgs",
    attribute_path = "haskell.packages.ghc822.ghc",
    build_file = "@ai_formation_hazel//:BUILD.ghc",
)

nixpkgs_package(
  name = "c2hs",
  repository = "@nixpkgs",
  attribute_path = "haskell.packages.ghc822.c2hs",
)

nixpkgs_package(
    name = "taglib",
    repository = "@nixpkgs",
    attribute_path = "taglib",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_cc_import")

filegroup (
  name = "lib",
  srcs = glob([
    "lib/libtag_c.so",
    "lib/libtag_c.dylib",
  ]),
)
""",
)

nixpkgs_package(
    name = "libsndfile.out",
    repository = "@nixpkgs",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
  name = "lib",
  srcs = glob([
    "lib/libsndfile.so",
    "lib/libsndfile.dylib",
  ]),
)
"""
)

nixpkgs_package(
    name = "libsndfile.dev",
    repository = "@nixpkgs",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
  name = "headers",
  srcs = glob([
    "include/*.h",
  ]),
)
"""
)

nixpkgs_package(
  name = "postgresql",
  repository = "@nixpkgs",
  build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup (
  name = "lib",
  srcs = glob([
    "lib/libecpg.so",
    "lib/libecpg.dylib",
  ]),
)

filegroup (
  name = "headers",
  srcs = glob([
    "include/*.h",
    "include/**/*.h",
  ]),
)
"""
)

nixpkgs_package(
  name = "glib_locales",
  repository = "@nixpkgs",
  attribute_path = "glibcLocales",
  build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
  name = "locale-archive",
  srcs = ["lib/locale/locale-archive"],
)
"""
)

register_toolchains(
    "@ghc//:ghc",
    "//:doctest",
)

load("//:hazel.bzl", "hazel_repositories",
     "hazel_custom_package_hackage",
     "hazel_custom_package_github",
)

hazel_custom_package_hackage(
  package_name = "zlib",
  version = "0.6.2",
)

hazel_custom_package_hackage(
  package_name = "zlib-bindings",
  version = "0.1.1.5",
)

hazel_custom_package_hackage(
  package_name = "vault",
  version = "0.3.1.1",
)

hazel_custom_package_hackage(
  package_name = "ghc-paths",
  version = "0.1.0.9",
)

hazel_custom_package_github(
  package_name = "text-metrics",
  github_user = "mrkkrp",
  github_repo = "text-metrics",
  repo_sha = "5d10b6f6ec4ff4b014e5e512f82d23e7606cc260",
)

hazel_custom_package_github(
  package_name = "conduit",
  github_user = "snoyberg",
  github_repo = "conduit",
  strip_prefix = "conduit",
  repo_sha = "34db9267bb4f9dbdee45623944900062e7995d09",
)

hazel_custom_package_github(
  package_name = "wai-app-static",
  github_user = "FormationAI",
  github_repo = "wai",
  strip_prefix = "wai-app-static",
  repo_sha = "aaa0dca56231c060372004cda46d719ec6cc3ec5",
)

load("//:packages.bzl", "packages", "core_packages")

hazel_repositories(
    packages = packages,
    core_packages = core_packages,
    exclude_packages = [
      "conduit",
      "ghc-paths",
      "text-metrics",
      "vault",
      "wai-app-static",
      "zlib",
      "zlib-bindings",
    ],
    extra_cdeps = {
      "pq": "@//:pq",
    },
    extra_libs = {
      "tag_c": "@taglib//:lib",
      "sndfile": "@libsndfile.out//:lib",
    },
    extra_libs_hdrs = {
      "sndfile": "@libsndfile.dev//:headers",
    },
    extra_libs_strip_include_prefix = {
      "sndfile": "/external/libsndfile.dev/include",
    },
)
