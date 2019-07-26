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

set -euo pipefail

# ----------------------------------------------------------
# Find compiler

find_exe() {
    local exe="$1"
    local location

    location="$exe"
    if [[ -f "$location" ]]; then
        echo "$location"
        return
    fi

    location="${exe}.exe"
    if [[ -f "$location" ]]; then
        echo "$location"
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
        echo "$location"
        return
    fi

    location="$(rlocation "{:workspace:}/${exe}.exe")"
    if [[ -f "$location" ]]; then
        echo "$location"
        return
    fi
}

CC="$(find_exe "{:cc:}")"

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
    local arg="$1"
    if [[ "$arg" =~ ^\"(.*)\"[[:space:]]*$ ]]; then
        arg="${BASH_REMATCH[1]}"
        arg="${arg//\\\"/\"}"
        arg="${arg//\\\\/\\}"
    fi
    echo "$arg"
}

add_arg() {
    quote_arg "$1" >> "$RESPONSE_FILE"
}

# ----------------------------------------------------------
# Parse arguments

IN_RESPONSE_FILE=
LIB_DIR_COMING=

shorten_path() {
    local input="$1"
    local shortest="$input"

    if [[ ! -e "$shortest" ]]; then
        # realpath fails if the file does not exist.
        echo "$shortest"
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

    echo "$shortest"
}

handle_lib_dir() {
    local lib_dir="$1"
    add_arg "-L$(shorten_path "$lib_dir")"
}

handle_arg() {
    local arg="$1"
    if [[ $IN_RESPONSE_FILE = 1 ]]; then
        arg="$(unquote_arg "$arg")"
    fi
    if [[ $LIB_DIR_COMING = 1 ]]; then
        LIB_DIR_COMING=
        handle_lib_dir "$arg"
    elif [[ "$arg" =~ ^@(.*)$ ]]; then
        IN_RESPONSE_FILE=1
        while read line; do
            handle_arg "$line"
        done < "${BASH_REMATCH[1]}"
        IN_RESPONSE_FILE=
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
