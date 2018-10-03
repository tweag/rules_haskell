{ pkgs ? import ./nixpkgs {} }:

with pkgs;
with darwin.apple_sdk.frameworks;

# XXX On Darwin, workaround
# https://github.com/NixOS/nixpkgs/issues/42059. See also
# https://github.com/NixOS/nixpkgs/pull/41589.
let cc = runCommand "cc-wrapper-bazel" {
    buildInputs = [ stdenv.cc makeWrapper ];
  }
  ''
    mkdir -p $out/bin

    # Copy the content of stdenv.cc
    for i in ${stdenv.cc}/bin/*
    do
      ln -sf $i $out/bin
    done

    # Override clang
    rm $out/bin/clang

    makeWrapper ${stdenv.cc}/bin/clang $out/bin/clang \
      --add-flags "-isystem ${llvmPackages.libcxx}/include/c++/v1 \
                   -F${CoreFoundation}/Library/Frameworks \
                   -F${CoreServices}/Library/Frameworks \
                   -F${Security}/Library/Frameworks \
                   -F${Foundation}/Library/Frameworks \
                   -L${libcxx}/lib \
                   -L${darwin.libobjc}/lib"
 '';

  mkShell = pkgs.mkShell.override {
    stdenv = if stdenv.isDarwin then overrideCC stdenv cc else stdenv;
  };
in
mkShell {
  # XXX: hack for macosX, this flags disable bazel usage of xcode
  # Note: this is set even for linux so any regression introduced by this flag
  # will be catched earlier
  # See: https://github.com/bazelbuild/bazel/issues/4231
  BAZEL_USE_CPP_ONLY_TOOLCHAIN=1;

  buildInputs = [
    binutils
    go
    graphviz
    nix
    which
    python
    python36Packages.sphinx
    zip
    unzip
    bazel
  ];
}
