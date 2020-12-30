let
  # 2020-12-30
  sha256 = "1lkmjqddqrfvypxz0y0cf4z3xa1j74mvd9ryl66lwahnijssgvkx";
  rev = "f6188ca545660da0aa722e7a50c1a3952da0a5ef";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
