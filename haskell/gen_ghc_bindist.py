#!/usr/bin/env python3

# This is a happy-path tool to download the bindist
# download paths and hashes, for maintainers.
# It uses the hashes provided by download.haskell.org.

import pprint
import sys
from urllib.request import urlopen
from distutils.version import StrictVersion

# All GHC versions we generate.
# `version` is the version number
# `distribution_version` is a corrected name
# (sometimes bindists have errors and are updated by new bindists)
# `ignore_prefixes` is the prefix of files to ignore
# `ignore_suffixes` is the suffix of files to ignore
VERSIONS = [
    { "version": "9.2.1",
      "ignore_suffixes": [".bz2", ".lz", ".zip"] },
    { "version": "9.0.2",
      "ignore_prefixes": ["ghc-9.0.2a"],
      "ignore_suffixes": [".bz2", ".lz", ".zip"] },
    { "version": "9.0.1",
      "ignore_suffixes": [".bz2", ".lz", ".zip"] },
    { "version": "8.10.7",
      "ignore_suffixes": [".bz2", ".lz", ".zip"] },
    { "version": "8.10.4" },
    { "version": "8.10.3" },
    { "version": "8.10.2" },
    { "version": "8.10.1",
      "ignore_suffixes": [".lz"] },
    { "version": "8.8.4" },
    { "version": "8.8.3",
      "ignore_suffixes": [".bz2", ".lz", ".zip"] },
    { "version": "8.8.2" },
    { "version": "8.8.1" },
    { "version": "8.6.5" },
    { "version": "8.6.4" },
    { "version": "8.6.3" },
    { "version": "8.6.2" },
    { "version": "8.4.4" },
    { "version": "8.4.3" },
    { "version": "8.4.2" },
    { "version": "8.4.1" },
    { "version": "8.2.2" },
    { "version": "8.0.2",
      "ignore_suffixes": [".patch"] },
    { "version": "7.10.3",
      "distribution_version": "7.10.3b",
      "ignore_prefixes": ["ghc-7.10.3-", "ghc-7.10.3a-"],
      "ignore_suffixes": [".bz2", ".patch" ] }
]

# All architectures we generate.
# bazel: bazel name
# upstream: list of download.haskell.org name
ARCHES = [
    { "bazel": "linux_amd64",
      "upstream": ["x86_64-deb8-linux", "x86_64-deb9-linux", "x86_64-deb10-linux"], },
    { "bazel": "darwin_amd64",
      "upstream": ["x86_64-apple-darwin"] },
    { "bazel": "darwin_arm64",
      "upstream": ["aarch64-apple-darwin"] },
    { "bazel": "windows_amd64",
      "upstream": ["x86_64-unknown-mingw32"] },
]


# An url to a bindist tarball.
def link_for_tarball(arch, version):
    return "https://downloads.haskell.org/~ghc/{ver}/ghc-{ver}-{arch}.tar.xz".format(
        ver = version,
        arch = arch,
    )

# An url to a version's tarball hashsum file.
# The files contain the hashsums for all arches.
def link_for_sha256_file(version):
    return "https://downloads.haskell.org/~ghc/{ver}/SHA256SUMS".format(
        ver = version
    )

# Parses the tarball hashsum file for a distribution version.
def parse_sha256_file(content, version, url):
    res = {}
    errs = []
    for line in content:
        # f5763983a26dedd88b65a0b17267359a3981b83a642569b26334423f684f8b8c  ./ghc-8.4.3-i386-deb8-linux.tar.xz
        (hash, file_) = line.decode().strip().split("  ./")
        prefix = "ghc-{ver}-".format(ver = version.get("distribution_version", version['version']))
        suffix = ".tar.xz"

        # filter ignored files
        if   any([file_.startswith(p) for p in version.get("ignore_prefixes", [])]) \
          or any([file_.endswith(s)   for s in version.get("ignore_suffixes", [])]):
            continue

        if file_.startswith(prefix) and file_.endswith(suffix):
            # i386-deb8-linux
            name = file_[len(prefix):-len(suffix)]
            res[name] = hash
        else:
            errs.append("Can't parse the sha256 field for {ver}: {entry}".format(
                ver = version['version'], entry = line.strip()))

    if errs:
        eprint("Errors parsing file at " + url + ". Either fix or ignore the lines (ignore_suffixes/ignore_prefixes).")
        for e in errs:
            eprint(e)
        exit(1)

    return res

# Print to stderr.
def eprint(mes):
    print(mes, file = sys.stderr)

def select_one(xs, ys):
    """Select a single item from xs, prefer the first item also in ys."""
    items = [x for x in xs if x in ys]
    return items[0] if items else xs[0]

# Main.
if __name__ == "__main__":
    # Fetch all hashsum files
    # grab : { version: { arch: sha256 } }
    grab = {}
    for ver in VERSIONS:
        eprint("fetching " + ver['version'])
        url = link_for_sha256_file(ver['version'])
        res = urlopen(url)
        if res.getcode() != 200:
            eprint("download of {} failed with status {}".format(url, res.getcode()))
            sys.exit(1)
        else:
            grab[ver['version']] = parse_sha256_file(res, ver, url)

    # check whether any version is missing arches we need
    # errs : { version: set(missing_arches) }
    errs = {}
    for ver, hashes in grab.items():
      real_arches = frozenset(hashes.keys())
      upstreams = [select_one(a['upstream'], real_arches) for a in ARCHES]
      needed_arches = frozenset(upstreams)
      missing_arches = needed_arches.difference(real_arches)
      if missing_arches:
          errs[ver] = missing_arches
    if errs:
        for ver, missing in errs.items():
            print(
                "WARN: version {ver} is missing hashes for architectures {arches}".format(
                    ver = ver,
                    arches = ','.join(missing)),
                file=sys.stderr
            )

    # fetch the arches we need and create the GHC_BINDISTS dict
    # ghc_bindists : { version: { bazel_arch: (tarball_url, sha256_hash) } }
    ghc_bindists = {}
    for ver, hashes in grab.items():
        # { bazel_arch: (tarball_url, sha256_hash) }
        arch_dists = {}
        for arch in ARCHES:
            upstream = select_one(arch['upstream'], hashes)

            if upstream in hashes:
                arch_dists[arch['bazel']] = (
                    link_for_tarball(upstream, ver),
                    hashes[upstream]
                )
        ghc_bindists[ver] = arch_dists

    # Print to stdout. Be aware that you can't `> foo.bzl`,
    # because that truncates the source file which is needed
    # for bazel to run in the first place.
    print("""\
# Generated with `bazel run @rules_haskell//haskell:gen-ghc-bindist`
# To add a version or architecture, edit the constants in haskell/gen_ghc_bindist.py,
# regenerate the dict and copy it here.
GHC_BINDIST = \\""")
    print("{")
    for version in sorted(ghc_bindists.keys(), key=StrictVersion):
       print('    ', repr(version), end=': ')
       print(pprint.pformat(ghc_bindists[version], indent=8), end=',\n')
    print("}")
