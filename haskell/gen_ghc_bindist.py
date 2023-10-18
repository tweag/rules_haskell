#!/usr/bin/env python3

# This is a happy-path tool to download the bindist
# download paths and hashes, for maintainers.
# It uses the hashes provided by download.haskell.org.

import os
import json
import sys
from urllib.request import urlopen
from distutils.version import StrictVersion

# Sometimes bindists have errors and are updated by new bindists.
# This dict is used to keep a version -> corrected_version mapping.
VERSIONS_CORRECTED = {}

# All architectures we generate.
# bazel: bazel name
# upstream: list of download.haskell.org name
ARCHES = [
    {
        "bazel": "linux_amd64",
        "upstream": ["x86_64-deb8-linux", "x86_64-deb9-linux", "x86_64-deb10-linux"],
    },
    {
        "bazel": "linux_arm64",
        "upstream": ["aarch64-deb10-linux"],
    },
    {"bazel": "darwin_amd64", "upstream": ["x86_64-apple-darwin"]},
    {"bazel": "darwin_arm64", "upstream": ["aarch64-apple-darwin"]},
    {"bazel": "windows_amd64", "upstream": ["x86_64-unknown-mingw32"]},
]


# An url to a bindist tarball.
def link_for_tarball(arch, version):
    return "https://downloads.haskell.org/~ghc/{ver}/ghc-{ver}-{arch}.tar.xz".format(
        ver=version,
        arch=arch,
    )


# An url to a version's tarball hashsum file.
# The files contain the hashsums for all arches.
def link_for_sha256_file(version):
    return "https://downloads.haskell.org/~ghc/{ver}/SHA256SUMS".format(ver=version)


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
            name = file_[len(prefix) : -len(suffix)]
            res[name] = hash

    if not res:
        eprint(
            f"Errors parsing file at {url}. Could not find entries for {prefix}…{suffix}"
        )
        exit(1)

    return res


# Print to stderr.
def eprint(mes):
    print(mes, file=sys.stderr)


def select_one(xs, ys):
    """Select a single item from xs, prefer the first item also in ys."""
    items = [x for x in xs if x in ys]
    return items[0] if items else xs[0]


def fetch_hashsums(versions):
    # Fetch all hashsum files
    # grab : { version: { arch: sha256 } }
    grab = {}
    for ver in versions:
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
        upstreams = [select_one(a["upstream"], real_arches) for a in ARCHES]
        needed_arches = frozenset(upstreams)
        missing_arches = needed_arches.difference(real_arches)
        if missing_arches:
            errs[ver] = missing_arches
    if errs:
        for ver, missing in errs.items():
            print(
                "WARN: version {ver} is missing hashes for architectures {arches}".format(
                    ver=ver, arches=",".join(missing)
                ),
                file=sys.stderr,
            )

    return grab


def fetch_bindists(grab):
    # fetch the arches we need and create the GHC_BINDISTS dict
    # ghc_bindists : { version: { bazel_arch: (tarball_url, sha256_hash) } }
    ghc_bindists = {}
    for ver, hashes in grab.items():
        # { bazel_arch: (tarball_url, sha256_hash) }
        arch_dists = {}
        for arch in ARCHES:
            upstream = select_one(arch["upstream"], hashes)

            if upstream in hashes:
                arch_dists[arch["bazel"]] = (
                    link_for_tarball(upstream, ver),
                    hashes[upstream],
                )
        ghc_bindists[ver] = arch_dists

    return ghc_bindists


# Main.
if __name__ == "__main__":
    working_directory = os.environ.get("BUILD_WORKING_DIRECTORY", ".")

    with open(
        os.path.join(working_directory, "haskell/private/ghc_bindist_generated.json"),
        "r+",
        encoding="utf-8",
    ) as json_file:
        ghc_versions = json.load(json_file)

        # All GHC versions we generate.
        versions = ghc_versions.keys()

        grab = fetch_hashsums(versions)

        ghc_bindists = fetch_bindists(grab)

        ghc_versions = {
            version: ghc_bindists[version]
            for version in sorted(ghc_bindists.keys(), key=StrictVersion)
        }

        json_file.truncate(0)
        json_file.seek(0)
        json.dump(ghc_versions, json_file, indent=4)
        json_file.write("\n")
