{ ... }@args:
let
  # 2023-11-30
  sha256 = "sha256:06m9r85brk6pcghm63ksl2zsx9j1kbagic3fx2p656z20v1bd48z";
  rev = "781e2a9797ecf0f146e81425c822dca69fe4a348";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) args
