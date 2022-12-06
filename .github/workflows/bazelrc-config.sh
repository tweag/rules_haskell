#!/usr/bin/env bash

set -e -o pipefail -o nounset

if [ -z "$BUILDBUDDY_API_KEY" ]; then
    cache_setting='--noremote_upload_local_results'
else
    cache_setting="--remote_header=x-buildbuddy-api-key=$BUILDBUDDY_API_KEY"
fi

cat <<EOF
common --config=ci
build --build_metadata=REPO_URL=${GITHUB_REPOSITORY}
build --build_metadata=BRANCH_NAME=${GITHUB_REF_NAME}
build --build_metadata=COMMIT_SHA=${GITHUB_SHA}
build:ci --build_metadata=ROLE=CI
build $cache_setting
EOF
