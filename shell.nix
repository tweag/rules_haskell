{ pkgs ? import ./nixpkgs {}, docTools ? true }:

with pkgs;
mkShell {
  # XXX: hack for macosX, this flags disable bazel usage of xcode
  # Note: this is set even for linux so any regression introduced by this flag
  # will be catched earlier
  # See: https://github.com/bazelbuild/bazel/issues/4231
  BAZEL_USE_CPP_ONLY_TOOLCHAIN=1;

  buildInputs = [
    go
    nix
    which
    perl
    python3
    bazel
    # For stack_install.
    stack
    # Needed for @com_github_golang_protobuf, itself needed by buildifier.
    git
    # Needed to get correct locale for tests with encoding
    glibcLocales
    # to avoid CA certificate failures on macOS CI
    cacert
    # Needed for debug/linking_utils
    binutils
    # check the start script for problems
    shellcheck
    file
  ] ++ lib.optionals docTools [graphviz python36Packages.sphinx zip unzip];

  shellHook = ''
    # Add nix config flags to .bazelrc.local.
    #
    BAZELRC_LOCAL=".bazelrc.local"
    if [ ! -e "$BAZELRC_LOCAL" ]
    then
      ARCH=""
      if [ "$(uname)" == "Darwin" ]; then
        ARCH="darwin"
      elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
        ARCH="linux"
      fi
      echo "[!] It looks like you are using a ''${ARCH} nix-based system. In order to build this project, you need to add the two following host_platform entries to your .bazelrc.local file."
      echo ""
      echo "build --host_platform=@rules_haskell//haskell/platforms:''${ARCH}_x86_64_nixpkgs"
      echo "run --host_platform=@rules_haskell//haskell/platforms:''${ARCH}_x86_64_nixpkgs"
    fi

    # source bazel bash completion
    source ${pkgs.bazel}/share/bash-completion/completions/bazel
  '';
}
