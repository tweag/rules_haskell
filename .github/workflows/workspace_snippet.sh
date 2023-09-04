#!/usr/bin/env bash

set -o errexit -o nounset -o pipefail

# Set by GH actions, see
# https://docs.github.com/en/actions/learn-github-actions/environment-variables#default-environment-variables
TAG=${GITHUB_REF_NAME}
REPO_NAME=${GITHUB_REPOSITORY#*/}
PREFIX="${REPO_NAME}-${TAG:1}"
URL="https://github.com/${GITHUB_REPOSITORY}/archive/refs/tags/${TAG}.tar.gz"

if ! SHA=$( curl -sfL "${URL}" | shasum -a 256 | awk '{print $1}'); then
    echo "error: could not determine hash for ${URL}" >&2
    exit 1
fi

cat << EOF
WORKSPACE snippet:
\`\`\`starlark
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
http_archive(
    name = "${REPO_NAME}",
    sha256 = "${SHA}",
    strip_prefix = "${PREFIX}",
    url = "${URL}",
)
\`\`\`
EOF
