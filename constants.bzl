load("@os_info//:os_info.bzl", "is_linux", "is_nix_shell", "is_windows")

# Windows builds fail with the following error on CI
#
#   Access violation in generated code when writing 0x0
#
#    Attempting to reconstruct a stack trace...
#
#      Frame	Code address
#    * 0x461dab0	0x37f7b66 C:\users\vssadministrator\_bazel_vssadministrator\w3d6ug6o\execroot\rules_haskell\external\rules_haskell_ghc_windows_amd64\bin\ghc.exe+0x33f7b66
#    * 0x461dab8	0x3277bb9 C:\users\vssadministrator\_bazel_vssadministrator\w3d6ug6o\execroot\rules_haskell\external\rules_haskell_ghc_windows_amd64\bin\ghc.exe+0x2e77bb9
#    * 0x461dac0	0x3
#
# This seems to be an instance of https://gitlab.haskell.org/ghc/ghc/issues/17926.
# Until a fix is released we fall back to an older GHC release on Windows.
test_ghc_version = "8.8.3" if not is_windows else "8.6.5"
test_stack_snapshot = "lts-15.4" if not is_windows else "lts-14.4"
