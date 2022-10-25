{ ... }@args:
let
  # 2022-10-25
  sha256 = "1n3r4s53q2clynvd6v2css054kf0icyfhxgs79brqvmrsxa7d0db";
  rev = "6107f97012a0c134c5848125b5aa1b149b76d2c9";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) args
