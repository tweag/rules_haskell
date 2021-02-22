let
  # 2021-02-22
  sha256 = "1mgga54np22csagzaxfjq5hrgyv8y4igrl3f6z24fb39rvvx236w";
  rev = "11cd34cd592f917bab5f42e2b378ab329dee3bcf";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
