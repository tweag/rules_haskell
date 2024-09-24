#!/usr/bin/env bash
#
# Replace place holders for http_archive attrs for rules_haskell with actual values
#

function die() {
    echo "error: $*"
    exit 1
} >&2

set -euo pipefail || die "cannot set options"

function get_sha256() {
    curl -L "$1" | python -uc 'import hashlib, sys; print(hashlib.sha256(sys.stdin.buffer.read()).hexdigest())'
}

if [[ "${READTHEDOCS_VERSION_TYPE}" == branch && "${READTHEDOCS_VERSION_NAME}" == master ]]; then
    # a commit was pushed to the master branch
    url="https://github.com/tweag/rules_haskell/archive/${READTHEDOCS_GIT_COMMIT_HASH}.tar.gz"
    hash=$( get_sha256 "$url" )
    sed -i \
        -e '/name = "rules_haskell"/,/url = "/{' \
        -e '  s%x\{64\}%'"${hash}"'%; ' \
        -e '  s%/releases/download/vM[.]NN/rules_haskell-%/archive/%' \
        -e '  s%M[.]NN%'"${READTHEDOCS_GIT_COMMIT_HASH}"'%g ' \
        -e '}' \
        docs/haskell-use-cases.rst
elif [[ "${READTHEDOCS_VERSION_TYPE}" == tag && "${READTHEDOCS_GIT_IDENTIFIER}" == v[0-9]* ]]; then
    # a version tag was pushed
    version="${READTHEDOCS_GIT_IDENTIFIER#v}"
    url="https://github.com/tweag/rules_haskell/releases/download/v${version}/rules_haskell-${version}.tar.gz"
    hash=$( get_sha256 "$url" )

    sed -i \
        -e '/name = "rules_haskell"/,/url = "/{' \
        -e '  s%x\{64\}%'"${hash}"'%; ' \
        -e '  s%M[.]NN%'"${version}"'%g ' \
        -e '}' \
        docs/haskell-use-cases.rst
else
    die "cannot handle version type ${READTHEDOCS_VERSION_TYPE} / ${READTHEDOCS_VERSION_NAME}"
fi
