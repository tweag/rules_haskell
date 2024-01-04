#!/usr/bin/env bash

# MARK - Functions

is_macos() {
  local os
  if ! which uname >/dev/null ; then
    return 1
  fi
  os="$( uname -s )"
  [[ "${os}" == "Darwin" ]]
}

# MARK - Execute Bazel command

if is_macos ; then
  # On MacOS, do not use Xcode. Use the client's value if they provided one.
  [[ -z "${BAZEL_USE_CPP_ONLY_TOOLCHAIN:-}" ]] && export BAZEL_USE_CPP_ONLY_TOOLCHAIN=1
fi

# DEBUG BEGIN
echo >&2 "*** CHUCK $(basename "${BASH_SOURCE[0]}") Actual" 
# DEBUG END

"${BAZEL_REAL}" "${@}"
