#!/bin/bash
#
# Copyright 2015 The Bazel Authors. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# This is a wrapper script around gcc/clang that adjusts linker flags for
# Haskell library and binary targets.
#
# Load commands that attempt to load dynamic libraries relative to the working
# directory in their package output path (bazel-out/...) are converted to load
# commands relative to @rpath. rules_haskell passes the corresponding
# -Wl,-rpath,... flags itself.
#
# rpath commands that attempt to add rpaths relative to the working directory
# to look for libraries in their package output path (bazel-out/...) are
# omitted, since rules_haskell adds more appropriate rpaths itself.
#
# GHC generates intermediate dynamic libraries outside the build tree.
# Additional RPATH entries are provided for those to make dynamic library
# dependencies in the Bazel build tree available at runtime.
#
# See https://blogs.oracle.com/dipol/entry/dynamic_libraries_rpath_and_mac
# on how to set those paths for Mach-O binaries.
#
set -euo pipefail

INSTALL_NAME_TOOL="/usr/bin/install_name_tool"
OTOOL="/usr/bin/otool"

# Collect arguments to forward in a fresh response file.
RESPONSE_FILE="$(mktemp osx_cc_args_XXXX.rsp)"
rm_response_file() {
    rm -f "$RESPONSE_FILE"
}
trap rm_response_file EXIT

add_args() {
   # Add the given arguments to the fresh response file. We follow GHC's
   # example in storing one argument per line, wrapped in double quotes. Double
   # quotes in the argument itself are escaped.
   for arg in "$@"; do
       printf '"%s"\n' "${arg//\"/\\\"}" >> "$RESPONSE_FILE"
   done
}

# Collect library, library dir, and rpath arguments.
LIBS=()
LIB_DIRS=()
RPATHS=()

# Parser state.
# Parsing response file - unquote arguments.
QUOTES=
# Upcoming linker argument.
LINKER=
# Upcoming rpath argument.
RPATH=
# Upcoming install-name argument.
INSTALL=
# Upcoming output argument.
OUTPUT=

parse_arg() {
    # Parse the given argument. Decide whether to pass it on to the compiler,
    # and how it affects the parser state.
    local arg="$1"
    # Unquote response file arguments.
    if [[ "$QUOTES" = "1" && "$arg" =~ ^\"(.*)\"$ ]]; then
        # Take GHC's argument quoting into account when parsing a response
        # file. Note, no indication was found that GHC would pass multiline
        # arguments, or insert escape codes into the quoted arguments. If you
        # observe ill-formed arguments being passed to the compiler, then this
        # logic may need to be extended.
        arg="${BASH_REMATCH[1]}"
    fi
    # Parse given argument.
    if [[ "$OUTPUT" = "1" ]]; then
        # The previous argument was -o. Read output file.
        OUTPUT="$arg"
        add_args "$arg"
    elif [[ "$LINKER" = "1" ]]; then
        # The previous argument was -Xlinker. Read linker argument.
        if [[ "$RPATH" = "1" ]]; then
            # The previous argument was -rpath. Read RPATH.
            parse_rpath "$arg"
            RPATH=0
        elif [[ "$arg" = "-rpath" ]]; then
            # rpath is coming
            RPATH=1
        else
            # Unrecognized linker argument. Pass it on.
            add_args "-Xlinker" "$arg"
        fi
        LINKER=
    elif [[ "$INSTALL" = "1" ]]; then
        INSTALL=
        add_args "$arg"
    elif [[ "$arg" =~ ^@(.*)$ ]]; then
        # Handle response file argument. Parse the arguments contained in the
        # response file one by one. Take GHC's argument quoting into account.
        # Note, assumes that response file arguments are not nested in other
        # response files.
        QUOTES=1
        while read line; do
            parse_arg "$line"
        done < "${BASH_REMATCH[1]}"
        QUOTES=
    elif [[ "$arg" = "-install_name" ]]; then
        # Install name is coming. We don't use it, but it can start with an @
        # and be mistaken for a response file.
        INSTALL=1
        add_args "$arg"
    elif [[ "$arg" = "-o" ]]; then
        # output is coming
        OUTPUT=1
        add_args "$arg"
    elif [[ "$arg" = "-Xlinker" ]]; then
        # linker flag is coming
        LINKER=1
    elif [[ "$arg" =~ ^-l(.*)$ ]]; then
        LIBS+=("${BASH_REMATCH[1]}")
        add_args "$arg"
    elif [[ "$arg" =~ ^-L(.*)$ ]]; then
        LIB_DIRS+=("${BASH_REMATCH[1]}")
        add_args "$arg"
    elif [[ "$arg" =~ ^-Wl,-rpath,(.*)$ ]]; then
        parse_rpath "${BASH_REMATCH[1]}"
    else
        # Unrecognized argument. Pass it on.
        add_args "$arg"
    fi
}

parse_rpath() {
    # Parse the given -rpath argument and decide whether it should be
    # forwarded to the compiler/linker.
    local rpath="$1"
    if [[ "$rpath" =~ ^/ || "$rpath" =~ ^@ ]]; then
        # Absolute rpaths or rpaths relative to @loader_path or similar, are
        # passed on to the linker. Other relative rpaths are dropped, these
        # are auto-generated by GHC, but are useless because rules_haskell
        # constructs dedicated rpaths to the _solib or _hssolib directory.
        # See https://github.com/tweag/rules_haskell/issues/689
        add_args "-Wl,-rpath,$rpath"
        RPATHS+=("$rpath")
    fi
}

# Parse all given arguments.
for arg in "$@"; do
    parse_arg "$arg"
done

get_library_in() {
    # Find the given library in the given directory.
    # Returns empty string if the library is not found.
    local lib="$1"
    local dir="$2"
    local solib="${dir}${dir:+/}lib${lib}.so"
    local dylib="${dir}${dir:+/}lib${lib}.dylib"
    if [[ -f "$solib" ]]; then
        echo "$solib"
    elif [[ -f "$dylib" ]]; then
        echo "$dylib"
    fi
}

get_library_path() {
    # Find the given library in the specified library search paths.
    # Returns empty string if the library is not found.
    if [[ ${#LIB_DIRS[@]} -gt 0 ]]; then
        local libpath
        for libdir in "${LIB_DIRS[@]}"; do
            libpath="$(get_library_in "$1" "$libdir")"
            if [[ -n "$libpath" ]]; then
                echo "$libpath"
                return
            fi
        done
    fi
}

resolve_rpath() {
    # Resolve the given rpath. I.e. if it is an absolute path, just return it.
    # If it is relative to the output, then prepend the output path.
    local rpath="$1"
    if [[ "$rpath" =~ ^/ ]]; then
        echo "$rpath"
    elif [[ "$rpath" =~ ^@loader_path/(.*)$ || "$rpath" =~ ^@executable_path/(.*)$ ]]; then
        echo "$(dirname "$OUTPUT")/${BASH_REMATCH[1]}"
    else
        echo "$rpath"
    fi
}

get_library_rpath() {
    # Find the given library in the specified rpaths.
    # Returns empty string if the library is not found.
    if [[ ${#RPATHS[@]} -gt 0 ]]; then
        local libdir libpath
        for rpath in "${RPATHS[@]}"; do
            libdir="$(resolve_rpath "$rpath")"
            libpath="$(get_library_in "$1" "$libdir")"
            if [[ -n "$libpath" ]]; then
                echo "$libpath"
                return
            fi
        done
    fi
}

get_library_name() {
    # Get the "library name" of the given library.
    "$OTOOL" -D "$1" | tail -1
}

relpath() {
    # Find relative path from the first to the second path. Assuming the first
    # is a directory. If either is an absolute path, then we return the
    # absolute path to the second.
    local from="$1"
    local to="$2"
    if [[ "$to" =~ ^/ ]]; then
        echo "$to"
    elif [[ "$from" =~ ^/ ]]; then
        echo "$PWD/$to"
    else
        # Split path and store components in bash array.
        IFS=/ read -a fromarr <<<"$from"
        IFS=/ read -a toarr <<<"$to"
        # Drop common prefix.
        for ((i=0; i < ${#fromarr[@]}; ++i)); do
            if [[ "${fromarr[$i]}" != "${toarr[$i]}" ]]; then
                break
            fi
        done
        # Construct relative path.
        local common=$i
        local out=
        for ((i=$common; i < ${#fromarr[@]}; ++i)); do
            out="$out${out:+/}.."
        done
        for ((i=$common; i < ${#toarr[@]}; ++i)); do
            out="$out${out:+/}${toarr[$i]}"
        done
        echo $out
    fi
}

generate_rpath() {
    # Generate an rpath entry for the given library path.
    local rpath="$(relpath "$(dirname "$OUTPUT")" "$(dirname "$1")")"
    if [[ "$rpath" =~ ^/ ]]; then
        echo "$rpath"
    else
        # Relative rpaths are relative to the binary.
        echo "@loader_path${rpath:+/}$rpath"
    fi
}

if [[ ! "$OUTPUT" =~ ^bazel-out/ && ${#LIBS[@]} -gt 0 ]]; then
    # GHC generates temporary dynamic libraries during compilation outside of
    # the build directory. References to dynamic C libraries are broken in this
    # case. Here we add additional RPATHs to fix these references. The Hazel
    # package for swagger2 is an example that triggers this issue.
    for lib in "${LIBS[@]}"; do
        librpath="$(get_library_rpath "$lib")"
        if [[ -z "$librpath" ]]; then
            # The given library was not found in any of the rpaths.
            # Find it in the library search paths.
            libpath="$(get_library_path "$lib")"
            if [[ "$libpath" =~ ^bazel-out/ ]]; then
                # The library is Bazel generated and loaded relative to PWD.
                # Add an RPATH entry, so it is found at runtime.
                rpath="$(generate_rpath "$libpath")"
                parse_rpath "$rpath"
            fi
        fi
    done
fi

# Call the C++ compiler with the fresh response file.
%{cc} "@$RESPONSE_FILE"

if [[ ${#LIBS[@]} -gt 0 ]]; then
    # Replace load commands relative to the working directory, by load commands
    # relative to the rpath, if the library can be found relative to an rpath.
    for lib in "${LIBS[@]}"; do
        librpath="$(get_library_rpath "$lib")"
        if [[ -n "$librpath" ]]; then
            libname="$(get_library_name "$librpath")"
            if [[ "$libname" =~ ^bazel-out/ ]]; then
                "${INSTALL_NAME_TOOL}" -change \
                    "$libname" \
                    "@rpath/$(basename "$librpath")" \
                    "$OUTPUT"
            fi
        fi
    done
fi

# vim: ft=sh
