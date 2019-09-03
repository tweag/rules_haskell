let
  # 2019-09-03
  sha256 = "1wddvvhppsr5yzdjrwmdhgjjz9zc5r2ryiwab5lz4rwz1k2vvapp";
  rev = "5eef8c231ada09d9d2f298fed5ec4718ab75ce10";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
