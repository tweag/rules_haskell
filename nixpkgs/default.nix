{ ... }@args:
let
  # nixos-26.05 @ 2026-09-21
  sha256 = "sha256:09mdvg1652sq4bm8fanbqwqs5szwwgf0cdn0s0j0ylz2kbvnhcwq";
  rev = "6d663c0533ff269008fb84e45930151e37c99db9";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) args
