# CC toolchain wrapper
#
# Usage: cc_wrapper [ARG]...
#
# Wraps the C compiler of the Bazel CC toolchain. Transforms arguments to work
# around limitations of Bazel and GHC and passes those via response file to the C
# compiler.
#
# - Shortens library search paths to stay below maximum path length on Windows.
#
#     GHC generates library search paths that contain redundant up-level
#     references (..). This can exceed the maximum path length on Windows, which
#     will cause linking failures. This wrapper shortens library search paths to
#     avoid that issue.
#
# - Shortens include paths to stay below maximum path length.
#
#     GHC generates include paths that contain redundant up-level
#     references (..). This can exceed the maximum path length, which
#     will cause compiler failures. This wrapper shortens include paths
#     to avoid that issue.

set -euo pipefail

# ----------------------------------------------------------
# Find compiler

find_exe() {
    local -n location="$1"
    local exe="$2"

    location="$exe"
    if [[ -f "$location" ]]; then
        return
    fi

    location="${exe}.exe"
    if [[ -f "$location" ]]; then
        return
    fi

    # --- begin runfiles.bash initialization v2 ---
    # Copy-pasted from the Bazel Bash runfiles library v2.
    set -uo pipefail; f=bazel_tools/tools/bash/runfiles/runfiles.bash
    source "${RUNFILES_DIR:-/dev/null}/$f" 2>/dev/null || \
      source "$(grep -sm1 "^$f " "${RUNFILES_MANIFEST_FILE:-/dev/null}" | cut -f2- -d' ')" 2>/dev/null || \
      source "$0.runfiles/$f" 2>/dev/null || \
      source "$(grep -sm1 "^$f " "$0.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
      source "$(grep -sm1 "^$f " "$0.exe.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
      { echo>&2 "ERROR: cannot find $f"; exit 1; }; f=; set -e
    # --- end runfiles.bash initialization v2 ---

    location="$(rlocation "{:workspace:}/$exe")"
    if [[ -f "$location" ]]; then
        return
    fi

    location="$(rlocation "{:workspace:}/${exe}.exe")"
    if [[ -f "$location" ]]; then
        return
    fi
}

declare CC
find_exe CC "{:cc:}"

# ----------------------------------------------------------
# Handle response file

RESPONSE_FILE="$(mktemp rspXXXX)"
rm_response_file() {
    rm -f "$RESPONSE_FILE"
}
trap rm_response_file EXIT

quote_arg() {
    # Gcc expects one argument per line, surrounded by double quotes, with
    # inner double quotes escaped with backslash, and backslashes themselves
    # escaped.
    local arg="$1"
    arg="${arg//\\/\\\\}"
    arg="${arg//\"/\\\"}"
    printf '"%s"\n' "$arg"
}

unquote_arg() {
    local -n output="$1"
    local input="$2"
    if [[ "$input" =~ ^\"(.*)\"[[:space:]]*$ ]]; then
        input="${BASH_REMATCH[1]}"
        input="${input//\\\"/\"}"
        input="${input//\\\\/\\}"
    fi
    output="$input"
}

add_arg() {
    quote_arg "$1" >> "$RESPONSE_FILE"
}

# ----------------------------------------------------------
# Parse arguments

IN_RESPONSE_FILE=
INCLUDE_DIR_COMING=
INCLUDE_FLAG=
LIB_DIR_COMING=

shorten_path() {
    local -n shortest="$1"
    local input="$2"

    shortest="$input"
    if [[ ! -e "$shortest" ]]; then
        # realpath fails if the file does not exist.
        return
    fi

    local normalized="$(realpath "$shortest")"
    if [[ ${#normalized} -lt ${#shortest} ]]; then
        shortest="$normalized"
    fi

    local relative="$(realpath --relative-to="$PWD" "$shortest")"
    if [[ ${#relative} -lt ${#shortest} ]]; then
        shortest="$relative"
    fi
}

handle_include_dir() {
    local flag="$1"
    local include_dir="$2"
    local shortened
    shorten_path shortened "$include_dir"
    add_arg "$flag$shortened"
}

handle_lib_dir() {
    local lib_dir="$1"
    local shortened
    shorten_path shortened "$lib_dir"
    add_arg "-L$shortened"
}

handle_arg() {
    local arg="$1"
    if [[ $IN_RESPONSE_FILE = 1 ]]; then
        unquote_arg arg "$arg"
    fi
    if [[ $INCLUDE_DIR_COMING = 1 ]]; then
        INCLUDE_DIR_COMING=
        INCLUDE_FLAG=
        handle_include_dir "$INCLUDE_FLAG" "$arg"
    elif [[ $LIB_DIR_COMING = 1 ]]; then
        LIB_DIR_COMING=
        handle_lib_dir "$arg"
    elif [[ "$arg" =~ ^@(.*)$ ]]; then
        IN_RESPONSE_FILE=1
        while read -r line; do
            handle_arg "$line"
        done < "${BASH_REMATCH[1]}"
        IN_RESPONSE_FILE=
    elif [[ "$arg" =~ ^(-I|-iquote|-isystem|-idirafter)(.*)$ ]]; then
        handle_include_dir "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}"
    elif [[ "$arg" = -I || "$arg" = -iquote || "$arg" = -isystem || "$arg" = -idirafter ]]; then
        INCLUDE_DIR_COMING=1
        INCLUDE_FLAG="$arg"
    elif [[ "$arg" =~ ^-L(.*)$ || "$arg" =~ ^--library-path=(.*)$ ]]; then
        handle_lib_dir "${BASH_REMATCH[1]}"
    elif [[ "$arg" = -L || "$arg" = --library-path ]]; then
        LIB_DIR_COMING=1
    else
        add_arg "$arg"
    fi
}

for arg in "$@"; do
    handle_arg "$arg"
done

# ----------------------------------------------------------
# Call compiler

"$CC" "@$RESPONSE_FILE"

# vim: ft=sh
