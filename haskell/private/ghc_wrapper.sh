#!/usr/bin/env bash
set -ueo pipefail
export PATH=${PATH:-} # otherwise GCC fails on Windows

# Drop messages that GHC produces on features that we rely upon.
#
# "Loaded" is emitted when using GHC environment files, which we
# use as poor man's response files for GHC.
#
# "Warning: the following files ..." is produced when we tell GHC
# to load object files in the interpreter in the build action
# of haskell_module which doesn't do any linking.

drop_loaded_and_warning () {
    while IFS= read -r line
    do
        [[ $line =~ ^(Loaded|Warning: the following files would be used as linker inputs, but linking is not being done:) ]] || echo "$line"
    done
}

# The file given as first argument contains the name of the executable (so a path to GHC) and some options it uses.

# this is equivalent to 'readarray'. We do not use 'readarray' in order to
# support older bash versions.
while IFS= read -r line; do compile_flags+=("$line"); done < "$1"

# Detect if we are in the persistent worker mode
if [ "$2" == "--persistent_worker" ]
then
    # This runs our proof-of-concept implementation of a persistent worker
    # wrapping GHC. Not ready for production usage.
    exec "${compile_flags[@]}" --persistent_worker
else
    # The file given as second argument contains the options and names of the files passed to GHC.
    while IFS= read -r line; do extra_args+=("$line"); done < "$2"

    # We use the information gathered from those 2 files to compile the Haskell file.

    "${compile_flags[@]}" "${extra_args[@]}" 2>&1 \
        | drop_loaded_and_warning >&2

    if [ "$MUST_EXTRACT_ABI" = "true" ]
    then
        # The next step is to generate a human-readable interface file.

        # We generate the call to `ghc --show-iface module_name.hi` from the 2 files passed as third and forth argument.
        while IFS= read -r line; do compile_show_iface+=("$line"); done < "$3"
        while IFS= read -r line; do show_iface_args+=("$line"); done < "$4"

        # This is the generation of the readable interface.
        # The fifth argument contains the name of the file in which the human-readable interface is written.
        # The sixth argument contains the name of the file in which the ABI hash is written.
        "${compile_show_iface[@]}" "${show_iface_args[@]}" > "$5" 2>&1 \
            | drop_loaded_and_warning >&2

        # The last step is to extract the ABI hash from the textual version of the interface

        sed -n "s/^\s*ABI hash:\s*\(\S*\)$/\1/p" "$5" > "$6"
    fi
fi