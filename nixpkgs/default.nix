{ ... }@args:
let
  # 2024-01-09
  sha256 = "sha256:62eb8e0ff586e410bc873a8e7997ec802709e9a77b03ff2cba644d7019f9599a";
  rev = "a6671578ffa6336bfd1238f1b2a00662b5ab1b34";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) args
