with import ./. { config = {}; overlays = []; };
with darwin.apple_sdk.frameworks;

# XXX On Darwin, workaround
# https://github.com/NixOS/nixpkgs/issues/42059. See also
# https://github.com/NixOS/nixpkgs/pull/41589.
let
  darwincc = runCommand "cc-wrapper-bazel" {
    buildInputs = [ pkgs.stdenv.cc makeWrapper ];
  }
  ''
    mkdir -p $out/bin

    # Copy the content of pkgs.stdenv.cc
    for i in ${pkgs.stdenv.cc}/bin/*
    do
      ln -sf $i $out/bin
    done

    # Override clang
    rm $out/bin/clang

    makeWrapper ${pkgs.stdenv.cc}/bin/clang $out/bin/clang \
      --add-flags "-isystem ${llvmPackages.libcxx}/include/c++/v1 \
                   -F${CoreFoundation}/Library/Frameworks \
                   -F${CoreServices}/Library/Frameworks \
                   -F${Security}/Library/Frameworks \
                   -F${Foundation}/Library/Frameworks \
                   -L${libcxx}/lib \
                   -L${darwin.libobjc}/lib \
                   -Wno-error=unused-command-line-argument"
  '';
  linuxcc = runCommand "cc-wrapper-bazel" {
    buildInputs = [ pkgs.stdenv.cc makeWrapper ];
  }
  ''
    mkdir -p $out/bin

    # Copy the content of pkgs.stdenv.cc
    for i in ${pkgs.stdenv.cc}/bin/*
    do
      ln -sf $i $out/bin
    done

    # Override clang
    rm $out/bin/gcc

    makeWrapper ${pkgs.stdenv.cc}/bin/gcc $out/bin/gcc \
      --add-flags "-pie"
  '';
  stdenv = if pkgs.stdenv.isDarwin then overrideCC pkgs.stdenv darwincc else overrideCC pkgs.stdenv linuxcc;
in
buildEnv {
  name = "bazel-cc-toolchain";
  paths = [ stdenv.cc ] ++ (if stdenv.isDarwin then [ ] else [ binutils ]);
}
