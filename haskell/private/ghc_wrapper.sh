#!/usr/bin/env bash
set -ueo pipefail
export PATH=${PATH:-} # otherwise GCC fails on Windows

echo ">>>>> $@"

CAT=/nix/store/fs0h13zxb5hcv1vr6yf2ijzp943jb6hy-coreutils-8.31/bin/cat

$CAT << EOF > test-package-env
package-id base-4.14.0.0
package-id containers-0.6.2.1
EOF

$CAT << EOF > Main.hs
module Main where
main = putStrLn "hello world"
EOF

# GHC 8.10.1, from Nixpkgs
# See https://github.com/tweag/rules_haskell/commit/82ace43c74f098f67def7cbbb485f2bd737c5c26
# See https://github.com/tweag/rules_haskell/issues/1309
external/rules_haskell_ghc_nixpkgs/bin/ghc \
-Wunused-packages \
-package-env test-package-env \
Main.hs \
-main-is Main.main

# # this is equivalent to 'readarray'. We do not use 'readarray' in order to
# # support older bash versions.
# while IFS= read -r line; do compile_flags+=("$line"); done < "$1"
#
# # Detect if we are in the persistent worker mode
# if [ "$2" == "--persistent_worker" ]; then
#     # This runs our proof-of-concept implementation of a persistent worker
#     # wrapping GHC. Not ready for production usage.
#     exec "${compile_flags[@]}" --persistent_worker
# else
#     while IFS= read -r line; do extra_args+=("$line"); done < "$2"
#     "${compile_flags[@]}" "${extra_args[@]}" 2>&1 \
#       | while IFS= read -r line; do [[ $line =~ ^Loaded ]] || echo "$line"; done >&2
# fi
