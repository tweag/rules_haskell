#!/usr/bin/env bash

set -o errexit -o nounset -o pipefail

ARCHIVE="$1"

# Set by GH actions, see
# https://docs.github.com/en/actions/learn-github-actions/environment-variables#default-environment-variables
TAG=${GITHUB_REF_NAME}
REPO_NAME=${GITHUB_REPOSITORY#*/}
PREFIX="${REPO_NAME}-${TAG:1}"
URL="https://github.com/${GITHUB_REPOSITORY}/releases/download/$TAG/$ARCHIVE"

if ! SHA=$( shasum -a 256 "$ARCHIVE" | awk '{print $1}'); then
    echo "error: could not determine hash for ${ARCHIVE}" >&2
    exit 1
fi

cat << EOF
## Using bzlmod with Bazel 6

1. Enable with \`common --enable_bzlmod\` in \`.bazelrc\`.
2. Add to your \`MODULE.bazel\` file:

\`\`\`starlark
bazel_dep(name = "rules_haskell", version = "${TAG:1}")
\`\`\`

## Using WORKSPACE

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
