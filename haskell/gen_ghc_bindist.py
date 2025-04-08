#!/usr/bin/env python3

# This is a happy-path tool to download the bindist
# download paths and hashes, for maintainers.
# It uses the hashes provided by download.haskell.org.

import os
import json
import re
import sys
from collections import OrderedDict
from urllib.request import urlopen
from packaging.version import Version

# Sometimes bindists have errors and are updated by new bindists.
# This dict is used to keep a version -> corrected_version mapping.
VERSIONS_CORRECTED = {}

# All architectures we generate.
# bazel: bazel name
# upstream: corresponding os / arch on download.haskell.org
ARCHES = [
    {"bazel": "linux_amd64", "upstream": {"os": "linux", "arch": "x86_64"}},
    {"bazel": "linux_arm64", "upstream": {"os": "linux", "arch": "aarch64"}},
    {"bazel": "darwin_amd64", "upstream": {"os": "darwin", "arch": "x86_64"}},
    {"bazel": "darwin_arm64", "upstream": {"os": "darwin", "arch": "aarch64"}},
    {"bazel": "windows_amd64", "upstream": {"os": "mingw32", "arch": "x86_64"}},
]


# An url to a version's tarball hashsum file.
# The files contain the hashsums for all arches.
def link_for_sha256_file(version):
    return "https://downloads.haskell.org/~ghc/{ver}/SHA256SUMS".format(ver=version)


# Parses the tarball hashsum file for a distribution version.
def parse_sha256_file(content, version, url):
    res = []

    version = VERSIONS_CORRECTED.get(version, version)
    suffix = ".tar.xz"

    ghc_regex = re.compile(
        r"ghc-(?P<version>[^-]+)-(?P<arch>[^-]+)-(?P<dist>[^-]+)-(?P<os>[^-]+)(?:-(?P<variant>[^.]+))?\.tar\.xz"
    )

    for line in content:
        # f5763983a26dedd88b65a0b17267359a3981b83a642569b26334423f684f8b8c  ./ghc-8.4.3-i386-deb8-linux.tar.xz
        (hash, file_) = line.decode().strip().split("  ./")

        m = ghc_regex.match(file_)

        if m:
            v = m.group('version')
            arch = m.group('arch')
            dist = m.group('dist')
            variant = m.group('variant')
            os = m.group('os')

            if v == version:
                spec = {"os": os, "arch": arch, "dist": dist, "sha256": hash, "url": f"https://downloads.haskell.org/~ghc/{version}/{file_}" }
                if variant:
                    spec["variant"] = variant
                res.append(spec)

    if not res:
        eprint(
            f"Errors parsing file at {url}. Could not find entries for GHC version {version} with {suffix}"
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
    # grab : { version: { arch: "..", sha256: "..", dist: "..", variant = "..", url: "..." } }
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

    return grab


def fetch_bindists(grab):
    # fetch the arches we need and create the GHC_BINDISTS dict
    # ghc_bindists : { version: { bazel_arch: [ {url, sha256, dist, variant} ] } }
    ghc_bindists = {}
    for ver, infos in grab.items():
        # { bazel_arch: (tarball_url, sha256_hash) }
        arch_dists = {}
        for arch in ARCHES:
            spec = arch["upstream"]
            upstreams = [
                upstream
                for upstream in infos if all([spec[k] == upstream.get(k) for k in spec])
            ]

            if not upstreams:
                print(
                    "WARN: version {ver} is missing hashes for {arch} {os}".format(ver=ver, **spec),
                    file=sys.stderr,
                )

            else:
                arch_dists[arch["bazel"]] = [
                    { k: v for k, v in upstream.items() if k not in ["os", "arch"]}
                    for upstream in sorted(upstreams, key=lambda u: (u["dist"], u.get("variant", "")))
                ]
        ghc_bindists[ver] = OrderedDict(sorted(arch_dists.items()))

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

        ghc_versions = OrderedDict(
            (version, ghc_bindists[version])
            for version in sorted(ghc_bindists.keys(), key=Version)
        )

        json_file.truncate(0)
        json_file.seek(0)
        json.dump(ghc_versions, json_file, indent=4)
        json_file.write("\n")
