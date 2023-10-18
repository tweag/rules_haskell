#!/usr/bin/env python3

# This is a happy-path tool to download the bindist
# download paths and hashes, for maintainers.
# It uses the hashes provided by download.haskell.org.

import os
import json
import sys
from urllib.request import urlopen
from distutils.version import StrictVersion

# All GHC versions we generate.
VERSIONS = [
    "9.6.3",
    "9.6.2",
    "9.6.1",
    "9.4.7",
    "9.4.6",
    "9.4.5",
    "9.2.8",
    "9.2.5",
    "9.2.4",
    "9.2.3",
    "9.2.1",
    "9.0.2",
    "9.0.1",
    "8.10.7",
    "8.10.4",
    "8.10.3",
    "8.8.4",
    "8.6.5",
    "8.4.4",
    "8.4.3",
    "8.4.2",
    "8.4.1",
    "8.2.2",
]

# Sometimes bindists have errors and are updated by new bindists.
# This dict is used to keep a version -> corrected_version mapping.
VERSIONS_CORRECTED = {}

# All architectures we generate.
# bazel: bazel name
# upstream: list of download.haskell.org name
ARCHES = [
    { "bazel": "linux_amd64",
      "upstream": ["x86_64-deb8-linux", "x86_64-deb9-linux", "x86_64-deb10-linux"], },
    { "bazel": "linux_arm64",
      "upstream": ["aarch64-deb10-linux"], },
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

    prefix = "ghc-{ver}-".format(ver=VERSIONS_CORRECTED.get(version, version))
    suffix = ".tar.xz"

    for line in content:
        # f5763983a26dedd88b65a0b17267359a3981b83a642569b26334423f684f8b8c  ./ghc-8.4.3-i386-deb8-linux.tar.xz
        (hash, file_) = line.decode().strip().split("  ./")

        if file_.startswith(prefix) and file_.endswith(suffix):
            # i386-deb8-linux
            name = file_[len(prefix):-len(suffix)]
            res[name] = hash

    if not res:
        eprint(f"Errors parsing file at {url}. Could not find entries for {prefix}…{suffix}")
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
        eprint("fetching " + ver)
        url = link_for_sha256_file(ver)
        res = urlopen(url)
        if res.getcode() != 200:
            eprint("download of {} failed with status {}".format(url, res.getcode()))
            sys.exit(1)
        else:
            grab[ver] = parse_sha256_file(res, ver, url)

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

    ghc_versions = { version: ghc_bindists[version] for version in sorted(ghc_bindists.keys(), key=StrictVersion) }

    working_directory = os.environ.get("BUILD_WORKING_DIRECTORY", ".")

    with open(os.path.join(working_directory, "haskell/private/ghc_bindist_generated.json"), "w", encoding="utf-8") as json_file:
        json.dump(ghc_versions, json_file, indent=4)
        json_file.write('\n')


