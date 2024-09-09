#!/usr/bin/env bash
set -euo pipefail

TAG=$1
REPO_NAME=${GITHUB_REPOSITORY#*/}
# The prefix is chosen to match what GitHub generates for source archives
PREFIX="${REPO_NAME}-${TAG:1}"
ARCHIVE="${REPO_NAME}-${TAG:1}.tar.gz"
git archive --format=tar.gz --prefix="${PREFIX}/" -o $ARCHIVE HEAD
SHA=$(shasum -a 256 "$ARCHIVE" | awk '{print $1}')

cat << EOF
## Using Bzlmod with Bazel 6+

1. Enable with \`common --enable_bzlmod\` in \`.bazelrc\`.
2. Add to your \`MODULE.bazel\` file:

\`\`\`starlark
bazel_dep(name = "${REPO_NAME}", version = "${TAG:1}")
\`\`\`

## Using WORKSPACE

Paste this snippet into your \`WORKSPACE.bazel\` file:

\`\`\`starlark
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "${REPO_NAME}",
    sha256 = "${SHA}",
    strip_prefix = "$PREFIX",
    urls = ["https://github.com/$GITHUB_REPOSITORY/releases/download/$TAG/$ARCHIVE"],
)

load("@${REPO_NAME}//haskell:repositories.bzl", "${REPO_NAME}_dependencies")

${REPO_NAME}_dependencies()
\`\`\`
EOF
