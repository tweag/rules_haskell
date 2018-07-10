{pkgs ? import ./nixpkgs.nix{} }:

with pkgs;
with darwin.apple_sdk.frameworks;

stdenv.mkDerivation {
  name = "cc-wrapper-bazel";
  buildInputs = [ stdenv.cc makeWrapper ];
  phases = [ "fixupPhase" ];
  postFixup = ''
    mkdir -p $out/bin
    makeWrapper ${stdenv.cc}/bin/clang $out/bin/clang \
      --add-flags "-isystem ${llvmPackages.libcxx}/include \
                   -F${CoreFoundation}/Library/Frameworks \
                   -F${CoreServices}/Library/Frameworks \
                   -F${Security}/Library/Frameworks \
                   -F${Foundation}/Library/Frameworks \
                   -L${libcxx}/lib \
                   -L${darwin.libobjc}/lib"
 '';
  }
