{ ... }@args:
let
  # 2023-11-30
  sha256 = "sha256:1lm7rkcbr7gg5zp62bga8iqyhg5hsvcly95hq0p3mcv7zq8n3wc2";
  rev = "7144d6241f02d171d25fba3edeaf15e0f2592105";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) args
