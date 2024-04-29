{ ... }@args:
let
  # 2024-04-27
  sha256 = "sha256:1iy5vigbw2dx7rhzmsszc7d87sw545f0vw4kcwxk3mazxg9qrzgl";
  rev = "2b1f64b358f2cab62617f26b3870fd0ee375d848";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) args
