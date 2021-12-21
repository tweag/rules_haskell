{ ... }@args:
let
  # 2021-11-29
  sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
  rev = "a7ecde854aee5c4c7cd6177f54a99d2c1ff28a31";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) args
