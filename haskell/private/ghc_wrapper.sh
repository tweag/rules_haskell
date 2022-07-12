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

# This is the compilation of the Haskell file.
"${compile_flags[@]}" "${extra_args[@]}" 2>&1 \
    | while IFS= read -r line; do [[ $line =~ ^(Loaded|Warning: the following files would be used as linker inputs, but linking is not being done:) ]] || echo "$line"; done >&2

if [ "$MUST_EXTRACT_ABI" = "true" ]
then
    # The second step is to generate a human-readable interface file.

    # We generate the files containing the call to `ghc --show-iface module_name.hi`
    while IFS= read -r line; do compile_show_iface+=("$line"); done < "$3"
    while IFS= read -r line; do show_iface_args+=("$line"); done < "$4"

    # This is the generation of the readable interface.
    "${compile_show_iface[@]}" "${show_iface_args[@]}" > "$5" 2>&1 \
        | while IFS= read -r line; do [[ $line =~ ^(Loaded|Warning: the following files would be used as linker inputs, but linking is not being done:) ]] || echo "$line"; done >&2

    # The third step is to extract the ABI hash from the textual version of the interface

    sed -n "s/^\s*ABI hash:\s*\(\S*\)$/\1/p" "$5" > "$6"
fi
