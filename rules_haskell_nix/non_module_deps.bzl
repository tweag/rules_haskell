load("@rules_haskell//tools:os_info.bzl", "os_info")

def repositories(*, bzlmod):
    os_info(name = "os_info")

def _non_module_deps_impl(_ctx):
    repositories(bzlmod = True)

non_module_deps = module_extension(
    implementation = _non_module_deps_impl,
)
