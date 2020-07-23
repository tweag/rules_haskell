#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat >&2 <<'EOF'
usage: hie-bios [OPTION]... FILE

hie-bios bios cradle script.

Determine the required GHCi flags for the given Haskell source FILE.
Prints the flags to $HIE_BIOS_OUTPUT or stdout.

Must be executed from the Bazel workspace root.
I.e. the hie.yaml file must be located in the workspace root.

OPTIONS
  --from_binary=PAT   Target pattern to load as binary.
  --from_source=PAT   Target pattern to load from source.
  --build=ARG         Pass ARG to bazel build.
  --extra=ARG         Additional GHC option to produce.
EOF
}

RULES_HASKELL="@rules_haskell"
if [[ -n ${RULES_HASKELL_INTERNAL-} ]]; then
  # Beware of workspace prefix issues on build settings and aspects:
  # https://github.com/bazelbuild/bazel/issues/9177
  # https://github.com/bazelbuild/bazel/issues/11734
  RULES_HASKELL=""
fi

# Parse arguments
FROM_BINARY_SET=
FROM_SOURCE_SET=
SRC_SET=
SRC=
BUILD_ARGS=()
EXTRA_ARGS=()
for arg in "$@"; do
  case "$arg" in
    --from_binary=*)
      if [[ -n $FROM_BINARY_SET ]]; then
        echo -e "ERROR: --from_binary defined more than once. Use a comma-separated list instead.\n" >&2
        usage
        exit 1
      fi
      FROM_BINARY_SET=1
      BUILD_ARGS+=("--${RULES_HASKELL}//haskell:repl_from_binary=${arg#--from_binary=}")
      ;;
    --from_source=*)
      if [[ -n $FROM_SOURCE_SET ]]; then
        echo -e "ERROR: --from_source defined more than once. Use a comma-separated list instead.\n" >&2
        usage
        exit 1
      fi
      FROM_SOURCE_SET=1
      BUILD_ARGS+=("--${RULES_HASKELL}//haskell:repl_from_source=${arg#--from_source=}")
      ;;
    --build=*)
      BUILD_ARGS+=("${arg#--build=}")
      ;;
    --extra=*)
      EXTRA_ARGS+=("${arg#--extra=}")
      ;;
    *)
      if [[ -n $SRC_SET ]]; then
        echo -e "ERROR: Unexpected argument: $arg\n" >&2
        usage
        exit 1
      fi
      SRC_SET=1
      SRC="$arg"
      ;;
  esac
done
if [[ -z $SRC_SET ]]; then
  echo -e "ERROR: No source file specified.\n" >&2
  usage
  exit 1
fi

# Change into workspace directory when invoked from bazel run.
if [[ -n "${BUILD_WORKSPACE_DIRECTORY-}" ]]; then
  cd "$BUILD_WORKSPACE_DIRECTORY"
fi

# Determine file path relative to Bazel workspace root.
BAZEL_WORKSPACE="$(bazel info workspace)"
if command -v realpath &>/dev/null; then
  relative_path() {
    realpath --relative-to="$BAZEL_WORKSPACE" "$1"
  }
else
  relative_path() {
    python -c "import os.path, sys; sys.stdout.write(os.path.relpath(sys.argv[1], sys.argv[2]))" "$1" "$BAZEL_WORKSPACE"
  }
fi

# Find Bazel target related to a Haskell source file.
query_target() {
  local src_label="$(bazel query "$1")"
  bazel cquery \
    'kind("haskell_binary|haskell_library|haskell_test", attr("srcs", '"$src_label"', '"${src_label//:*/}:*"'))' \
    | awk '{ print $1; exit }'
}

# Determine GHCi flags for a given Bazel target.
hie_bios_flags() {
  local aspect="--aspects=@rules_haskell//haskell:repl.bzl%haskell_repl_aspect"
  if [[ -n ${RULES_HASKELL_INTERNAL-} ]]; then
    # Beware of workspace prefix issues on build settings and aspects:
    # https://github.com/bazelbuild/bazel/issues/9177
    # https://github.com/bazelbuild/bazel/issues/11734
    local aspect="--aspects=//haskell:repl.bzl%haskell_repl_aspect"
  fi
  bazel build "$1" \
    "${BUILD_ARGS[@]}" \
    "--aspects=${RULES_HASKELL}//haskell:repl.bzl%haskell_repl_aspect" \
    --output_groups=hie_bios \
    --experimental_show_artifacts \
    2>&1 \
    | awk '
      /^>>>/ {
        while ((getline line < substr($1, 4)) > 0) {
          print line
        }
        next
      }
      {
        print $0 > "/dev/stderr"
      }
    '
  IFS=$'\n'; echo "${EXTRA_ARGS[*]}"; unset IFS
}

if [[ ! -f "$SRC" ]]; then
  echo "ERROR: File not found: $SRC" >&2
  exit 1
fi
SRC="$(relative_path "$SRC")"
TARGET="$(query_target "$SRC")"
if [[ -z "$TARGET" ]]; then
  echo "No Bazel Haskell target found for '$SRC'." >&2
  exit 1
fi
if [[ -z "${HIE_BIOS_OUTPUT-}" ]]; then
  hie_bios_flags "$TARGET"
else
  hie_bios_flags "$TARGET" >"$HIE_BIOS_OUTPUT"
fi
