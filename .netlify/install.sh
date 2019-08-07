#!/bin/sh

set -eux

# XXX: This should be bazel 0.27
# However, starting from bazel 0.27, they link with a more recent
# glibc, they are based on ubuntu 16.04 instead of 14.04.
# Apparently, netlify does not have the right glibc for now
# So, using bazel 0.26 is a temporary solution until netlify fix its
# environment.
V=0.26.0

curl -LO https://github.com/bazelbuild/bazel/releases/download/$V/bazel-$V-installer-linux-x86_64.sh
chmod +x bazel-$V-installer-linux-x86_64.sh
./bazel-$V-installer-linux-x86_64.sh --user
