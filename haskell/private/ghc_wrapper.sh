#!/usr/bin/env bash
set -ueo pipefail
export PATH=${PATH:-} # otherwise GCC fails on Windows

drop_loaded_and_warning () {
    # Drop messages that GHC produces on features that we rely upon.
    #
    # "Loaded" is emitted when using GHC environment files, which we
    # use as poor man's response files for GHC.
    #
    # "Warning: the following files ..." is produced when we tell GHC
    # to load object files in the interpreter in the build action
    # of haskell_module which doesn't do any linking.
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

    (set -x ; "${compile_flags[@]}" "${extra_args[@]}") 2>&1 \
        | drop_loaded_and_warning >&2

    if [ "$MUST_EXTRACT_ABI" = "true" ]
    then
        # The next step is to generate a human-readable interface file.

        # We generate the call to `ghc --show-iface module_name.hi` from the file passed as third argument.
        while IFS= read -r line; do compile_show_iface+=("$line"); done < "$3"

        # The fourth argument contains the name of the file in which ABI hash should be written.
        readable_hi="$4.readable_hi"
        abi="$4"

        # This is the generation of the readable interface.
        "${compile_show_iface[@]}" > "$readable_hi" 2>&1 \
            | drop_loaded_and_warning >&2

        # The last step is to extract the ABI hash from the textual version of the interface

        sed -n "s/^\s*ABI hash:\s*\(\S*\)$/\1/p" "$readable_hi" > "$abi"

        # We then copy the interface_inputs_list in the unused_inputs_list.
        # The fifth argument contains the name of the file containing the list of interfaces
        #   which should be ignored by the caching mechanism to know if recompilation should be triggered.
        # The sixth argument contains the name of the file used as unused_inputs_list.
        cp "$5" "$6"
    fi
fi
