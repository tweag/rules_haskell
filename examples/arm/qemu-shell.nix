let
  pkgs = (import ./arm-cross.nix).pkgs;
  crossPkgs = pkgs.pkgsCross.aarch64-multiplatform;
in
pkgs.mkShell {
  LANG="C.UTF-8";
  LD_LIBRARY_PATH = "${crossPkgs.buildPackages.gcc-unwrapped}/aarch64-unknown-linux-gnu/lib64";
  buildInputs = [
    pkgs.qemu
  ];
}
