{ ... }:
let
  # 2020-05-25
  sha256 = "0ncsh5rkjbcdaksngmn7apiq1qhm5z1z6xa1x70svxq91znibp4f";
  rev = "571212eb839d482992adb05479b86eeb6a9c7b2f";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) { overlays = [ (import ./add_sandboxfs.nix) ]; }
