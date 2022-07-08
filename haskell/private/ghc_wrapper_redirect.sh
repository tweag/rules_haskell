#!/usr/bin/env bash
set -ueo pipefail
export PATH=${PATH:-} # otherwise GCC fails on Windows

# this is equivalent to 'readarray'. We do not use 'readarray' in order to
# support older bash versions.
while IFS= read -r line; do compile_flags+=("$line"); done < "$1"

# Drop messages that GHC produces on features that we rely upon.
#
# "Loaded" is emitted when using GHC environment files, which we
# use as poor man's response files for GHC.
#
# "Warning: the following files ..." is produced when we tell GHC
# to load object files in the interpreter in the build action
# of haskell_module which doesn't do any linking.
while IFS= read -r line; do extra_args+=("$line"); done < "$2"
"${compile_flags[@]}" "${extra_args[@]}" > "$3" 2>&1 \
    | while IFS= read -r line; do [[ $line =~ ^(Loaded|Warning: the following files would be used as linker inputs, but linking is not being done:) ]] || echo "$line"; done >&2
