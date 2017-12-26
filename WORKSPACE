workspace(name = "io_tweag_rules_haskell")

local_repository(
  name = "examples",
  path = "examples"
)

http_archive(
  name = "io_tweag_rules_nixpkgs",
  # Commit hash is current latest master.
  strip_prefix = "rules_nixpkgs-a300f574885c50430147e457d21ec22a9fe015f4",
  urls = ["https://github.com/tweag/rules_nixpkgs/archive/a300f574885c50430147e457d21ec22a9fe015f4.tar.gz"],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_package")

# Default toolchain
nixpkgs_package(
  name = "ghc",
  attribute_path = "haskell.packages.ghc822.ghc"
)

nixpkgs_package(name = "binutils")

# For tests
nixpkgs_package(name = "zlib")
