workspace(name = "ai_formation_hazel")

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-cd2ed701127ebf7f8f21d37feb1d678e4fdf85e5",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/cd2ed70.tar.gz"],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_git_repository", "nixpkgs_package")

nixpkgs_git_repository(
    name = "nixpkgs",
    # A revision of 17.09 that contains ghc-8.2.2:
    revision = "c33c5239f62b4855b14dc5b01dfa3e2a885cf9ca",
)

RULES_HASKELL_SHA = "b55ca991a9e58108932ff6c8b86fd141897391c1"
http_archive(
    name = "io_tweag_rules_haskell",
    urls = ["https://github.com/tweag/rules_haskell/archive/"
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
  name = "postgresql",
  repository = "@nixpkgs",
  build_file_content = """
package(default_visibility = ["//visibility:public"])
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_cc_import")

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

register_toolchains("@ghc//:ghc")

load("//:hazel.bzl", "hazel_repositories",
     "hazel_custom_package_hackage",
     "hazel_custom_package_github",
)

hazel_custom_package_hackage(
  package_name = "zlib",
  version = "0.6.2",
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

load("//:packages.bzl", "packages", "prebuilt_dependencies")

hazel_repositories(
    packages=packages,
    prebuilt_dependencies=prebuilt_dependencies,
    exclude_packages = ["zlib", "text-metrics", "conduit"],
    extra_libs = {
      "tag_c": "@taglib//:lib",
      "pq": "@postgresql//:lib",
    },
    extra_libs_hdrs = {
      "pq": "@postgresql//:headers",
    },
    extra_libs_strip_include_prefix = {
      "pq": "/external/postgresql/include",
    },
)
