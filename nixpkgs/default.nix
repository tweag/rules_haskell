{ ... }@args:
let
  # 2022-07-16
  sha256 = "sha256:07s0mp3z2ibk4zj8hw2pcac46dzi8rzgc8ih6p6lxl3fkw4bdvqv";
  rev = "365e1b3a859281cf11b94f87231adeabbdd878a2";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) args
