#!/usr/bin/env bash
set -ueo pipefail
export PATH=${PATH:-} # otherwise GCC fails on Windows

echo ">>>>> $@"

echo 223

external/rules_haskell_ghc_nixpkgs/bin/ghc \
-pgma \
bazel-out/host/bin/haskell/cc_wrapper-python \
-pgmc \
bazel-out/host/bin/haskell/cc_wrapper-python \
-pgml \
bazel-out/host/bin/haskell/cc_wrapper-python \
-pgmP \
bazel-out/host/bin/haskell/cc_wrapper-python \
-optc-fno-stack-protector \
-optP-E \
-optP-undef \
-optP-traditional \
-static \
-v0 \
-no-link \
-fPIC \
-hide-all-packages \
-Wmissing-home-modules \
-odir \
bazel-out/k8-fastbuild/bin/tests/unused_packages/_obj/binary-simple \
-hidir \
bazel-out/k8-fastbuild/bin/tests/unused_packages/_iface/binary-simple \
-optc-U_FORTIFY_SOURCE \
-optc-fstack-protector \
-optc-Wall \
-optc-Wunused-but-set-parameter \
-optc-Wno-free-nonheap-object \
-optc-fno-omit-frame-pointer \
-optc-fno-canonical-system-headers \
-optc-Wno-builtin-macro-redefined \
-optc-D__DATE__="redacted" \
-optc-D__TIMESTAMP__="redacted" \
-optc-D__TIME__="redacted" \
-opta-U_FORTIFY_SOURCE \
-opta-fstack-protector \
-opta-Wall \
-opta-Wunused-but-set-parameter \
-opta-Wno-free-nonheap-object \
-opta-fno-omit-frame-pointer \
-opta-fno-canonical-system-headers \
-opta-Wno-builtin-macro-redefined \
-opta-D__DATE__="redacted" \
-opta-D__TIMESTAMP__="redacted" \
-opta-D__TIME__="redacted" \
-XStandaloneDeriving \
-threaded \
-DTESTS_TOOLCHAIN_COMPILER_FLAGS \
-XNoOverloadedStrings \
-Werror \
-Wunused-packages \
-hide-all-packages \
-fno-version-macros \
-package-env \
bazel-out/k8-fastbuild/bin/tests/unused_packages/compile-package_env-binary-simple \
-hide-all-plugin-packages \
-optP@bazel-out/k8-fastbuild/bin/tests/unused_packages/optp_args_binary-simple \
tests/unused_packages/Main.hs \
-main-is \
Main.main

# # this is equivalent to 'readarray'. We do not use 'readarray' in order to
# # support older bash versions.
# while IFS= read -r line; do compile_flags+=("$line"); done < "$1"
#
# # Detect if we are in the persistent worker mode
# if [ "$2" == "--persistent_worker" ]; then
#     # This runs our proof-of-concept implementation of a persistent worker
#     # wrapping GHC. Not ready for production usage.
#     exec "${compile_flags[@]}" --persistent_worker
# else
#     while IFS= read -r line; do extra_args+=("$line"); done < "$2"
#     "${compile_flags[@]}" "${extra_args[@]}" 2>&1 \
#       | while IFS= read -r line; do [[ $line =~ ^Loaded ]] || echo "$line"; done >&2
# fi
