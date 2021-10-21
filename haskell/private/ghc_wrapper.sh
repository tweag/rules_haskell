#!/usr/bin/env bash
set -ueo pipefail
export PATH=${PATH:-} # otherwise GCC fails on Windows

# this is equivalent to 'readarray'. We do not use 'readarray' in order to
# support older bash versions.
while IFS= read -r line; do compile_flags+=("$line"); done < "$1"

# Detect if we are in the persistent worker mode
if [ "$2" == "--persistent_worker" ]; then
    # This runs our proof-of-concept implementation of a persistent worker
    # wrapping GHC. Not ready for production usage.
    exec "${compile_flags[@]}" --persistent_worker
else
    while IFS= read -r line; do extra_args+=("$line"); done < "$2"
    icopy=""
    for((i=0;i+2<${#extra_args[@]};i++)); do
        case "${extra_args[$i]}" in
            --copy-outputs)
                icopy=$i
                SRC=${extra_args[$(($i+1))]}
                OUT=${extra_args[$(($i+2))]}
                extra_args=("${extra_args[@]:0:$i}" "${extra_args[@]:$(($i+3))}")
                break
                ;;
            *)
                ;;
        esac
    done

    "${compile_flags[@]}" "${extra_args[@]}" 2>&1 \
      | while IFS= read -r line; do [[ $line =~ ^Loaded ]] || echo "$line"; done >&2

    if [ ! -z "$icopy" ]; then
        cp $SRC.o $OUT.o
        cp $SRC.dyn_o $OUT.dyn_o
        cp $SRC.hi $OUT.hi
        cp $SRC.dyn_hi $OUT.dyn_hi
    fi
fi
