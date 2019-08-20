#!/usr/bin/env bash
set -ueo pipefail
export PATH=${PATH:-} # otherwise GCC fails on Windows

# this is equivalent to 'readarray'. We do not use 'readarray' in order to
# support older bash versions.
while IFS= read -r line; do compile_flags+=("$line"); done < $1

# Detect if we are in the persistent worker mode
if [ "$2" == "--persistent_worker" ]; then
    compile_flags=("${compile_flags[@]:1}")  # remove ghc executable
    # This is a proof-of-concept implementation, not ready for production usage:
    # it assumes https://github.com/tweag/bazel-worker/ installed globally as ~/bin/worker
    exec ~/bin/worker ${compile_flags[@]} --persistent_worker
else
    while IFS= read -r line; do extra_args+=("$line"); done < "$2"
    "${compile_flags[@]}" "${extra_args[@]}" 2>&1 \
      | while IFS= read -r line; do [[ $line =~ ^Loaded ]] || echo "$line"; done >&2
fi
