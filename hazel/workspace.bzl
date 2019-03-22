"""Workspace rules (setup)"""

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_package")

load("@ai_formation_hazel//:hazel.bzl",
     "hazel_extra_packages",
     "hazel_repositories",
     "hazel_custom_package_hackage",
     "hazel_custom_package_github",
)

load("@ai_formation_hazel//:packages.bzl", "packages", "core_packages")

def hazel_setup():
    """Setup the Hazel WORKSPACE.

    This is factored out of hazel/WORKSPACE so that it can be re-used in
    rules_haskell's WORKSPACE. It is necessary to setup Hazel in
    rules_haskell's WORKSPACE, because Bazel does not properly support nested
    workspaces.
    """

    nixpkgs_package(
        name = "taglib",
        attribute_path = "taglib",
        build_file_content = """
package(default_visibility = ["//visibility:public"])

cc_library(
  name = "taglib",
  srcs = glob([
    "lib/libtag_c.so*",
    "lib/libtag_c.dylib",
  ]),
)
""",
        repository = "@nixpkgs",
    )

    nixpkgs_package(
      name = "postgresql",
      repository = "@nixpkgs",
      build_file_content = """
package(default_visibility = ["//visibility:public"])

cc_library(
  name = "postgresql",
  srcs = glob([
    "lib/libecpg.so*",
    "lib/libecpg.dylib",
  ]),
  hdrs = glob([
    "include/*.h",
    "include/**/*.h",
  ]),
  strip_include_prefix = "include",
)
"""
    )

    hazel_custom_package_hackage(
      package_name = "zlib",
      version = "0.6.2",
      build_file = "@ai_formation_hazel//third_party/haskell:BUILD.zlib",
    )

    hazel_custom_package_hackage(
      package_name = "vault",
      version = "0.3.1.1",
      build_file = "@ai_formation_hazel//third_party/haskell:BUILD.vault",
    )

    hazel_custom_package_hackage(
      package_name = "ghc-paths",
      version = "0.1.0.9",
      build_file = "@ai_formation_hazel//third_party/haskell:BUILD.ghc-paths",
    )

    hazel_custom_package_github(
      package_name = "text-metrics",
      github_user = "mrkkrp",
      github_repo = "text-metrics",
      repo_sha = "5d10b6f6ec4ff4b014e5e512f82d23e7606cc260",
      build_file = "@ai_formation_hazel//third_party/haskell:BUILD.text-metrics",
    )

    hazel_custom_package_github(
      package_name = "wai-app-static",
      github_user = "FormationAI",
      github_repo = "wai",
      strip_prefix = "wai-app-static",
      repo_sha = "9217512fae1d6c2317447b257f478005efb55ef7",
      build_file = "@ai_formation_hazel//third_party/haskell:BUILD.wai-app-static",
    )

    hazel_repositories(
        packages = hazel_extra_packages(
            pkgs = packages,
            extra_pkgs = {
                "unix-time": {"version": "0.4.5", "sha256": "fe7805c62ad682589567afeee265e6e230170c3941cdce479a2318d1c5088faf"},
            },
        ),
        core_packages = core_packages,
        exclude_packages = [
          "ghc-paths",
          "text-metrics",
          "vault",
          "wai-app-static",
          "zlib",
        ],
        extra_libs = {
          "pq": "@postgresql",
          "tag_c": "@taglib",
        },
    )
