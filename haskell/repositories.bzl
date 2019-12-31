"""Workspace rules (repositories)"""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def rules_haskell_dependencies():
    """Provide all repositories that are necessary for `rules_haskell` to
    function.
    """
    excludes = native.existing_rules().keys()

    if "platforms" not in excludes:
        http_archive(
            name = "platforms",
            sha256 = "23566db029006fe23d8140d14514ada8c742d82b51973b4d331ee423c75a0bfa",
            strip_prefix = "platforms-46993efdd33b73649796c5fc5c9efb193ae19d51",
            urls = ["https://github.com/bazelbuild/platforms/archive/46993efdd33b73649796c5fc5c9efb193ae19d51.tar.gz"],
        )

    if "bazel_skylib" not in excludes:
        http_archive(
            name = "bazel_skylib",
            sha256 = "eb5c57e4c12e68c0c20bc774bfbc60a568e800d025557bc4ea022c6479acc867",
            strip_prefix = "bazel-skylib-0.6.0",
            urls = ["https://github.com/bazelbuild/bazel-skylib/archive/0.6.0.tar.gz"],
        )

    if "rules_cc" not in excludes:
        http_archive(
            name = "rules_cc",
            sha256 = "dafda2ff2a913028ce1718253b6b2f353b2d2163470f3069ca810a0d8d55a5a9",
            strip_prefix = "rules_cc-cd7e8a690caf526e0634e3ca55b10308ee23182d",
            urls = ["https://github.com/bazelbuild/rules_cc/archive/cd7e8a690caf526e0634e3ca55b10308ee23182d.tar.gz"],
        )

    if "rules_sh" not in excludes:
        http_archive(
            name = "rules_sh",
            sha256 = "2613156e96b41fe0f91ac86a65edaea7da910b7130f2392ca02e8270f674a734",
            strip_prefix = "rules_sh-0.1.0",
            urls = ["https://github.com/tweag/rules_sh/archive/v0.1.0.tar.gz"],
        )

    if "io_tweag_rules_nixpkgs" not in excludes:
        http_archive(
            name = "io_tweag_rules_nixpkgs",
            sha256 = "f5af641e16fcff5b24f1a9ba5d93cab5ad26500271df59ede344f1a56fc3b17d",
            strip_prefix = "rules_nixpkgs-0.6.0",
            urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.6.0.tar.gz"],
        )

def haskell_repositories():
    """DEPRECATED alias for rules_haskell_dependencies"""
    rules_haskell_dependencies()
