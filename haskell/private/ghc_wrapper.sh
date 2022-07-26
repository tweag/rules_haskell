#!/usr/bin/env bash
set -ueo pipefail
export PATH=${PATH:-} # otherwise GCC fails on Windows

# Detect if we are in the persistent worker mode
PERSISTENT_WORKER=0
for i in "$@"; do
    if [ "$i" == "--persistent_worker" ]; then
        PERSISTENT_WORKER=1
        break
    fi
done

if [ $PERSISTENT_WORKER == 1 ]; then
    # This runs our proof-of-concept implementation of a persistent worker
    # wrapping GHC.
    #
    # The path to "$1" is passed in the invocation so the persistent worker
    # can start the background workers.
    exec "$1" "$1" "${@:2}"
else
    # Drop messages that GHC produces on features that we rely upon.
    #
    # "Loaded" is emitted when using GHC environment files, which we
    # use as poor man's response files for GHC.
    #
    # "Warning: the following files ..." is produced when we tell GHC
    # to load object files in the interpreter in the build action
    # of haskell_module which doesn't do any linking.
    #
    # "${2:1}" drops the '@' sign at the begining of the filepath
    while IFS= read -r line; do extra_args+=("$line"); done < "${2:1}"
    "$1" "${extra_args[@]}" 2>&1 \
      | while IFS= read -r line; do [[ $line =~ ^(Loaded|Warning: the following files would be used as linker inputs, but linking is not being done:) ]] || echo "$line"; done >&2
fi
