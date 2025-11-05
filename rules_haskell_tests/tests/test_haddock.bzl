""" Compute expected results for the //tests:test-haddock test"""

load("@os_info//:os_info.bzl", "cpu_value", "is_darwin", "is_linux", "is_nix_shell", "is_windows")

def expected_values(test_ghc_version):
    if test_ghc_version == "9.2.8":
        return [
            "haddock/array-0.5.4.0",
            "haddock/base-4.16.4.0",
            "haddock/deepseq-1.4.6.1",
            "haddock/ghc-bignum-1.2",
            "haddock/ghc-prim-0.8.0",
            "haddock/index",
            "haddock/pretty-1.1.3.6",
            "haddock/template-haskell-2.18.0.0",
            "haddock/testsZShaddockZShaddock-lib-a",
            "haddock/testsZShaddockZShaddock-lib-b",
            "haddock/testsZShaddockZShaddock-lib-deep",
            "haddock/ghc-boot-th-9.2.8",
        ]
    elif test_ghc_version == "9.4.8":
        return [
            "haddock/array-0.5.4.0",
            "haddock/base-4.17.2.1",
            "haddock/deepseq-1.4.8.0",
            "haddock/ghc-bignum-1.3",
            "haddock/ghc-prim-0.9.1",
            "haddock/index",
            "haddock/pretty-1.1.3.6",
            "haddock/template-haskell-2.19.0.0",
            "haddock/testsZShaddockZShaddock-lib-a",
            "haddock/testsZShaddockZShaddock-lib-b",
            "haddock/testsZShaddockZShaddock-lib-deep",
            "haddock/ghc-boot-th-9.4.8",
        ]
    elif test_ghc_version == "9.6.5":
        return [
            "haddock/array-0.5.6.0",
            "haddock/base-4.18.2.1",
            "haddock/deepseq-1.4.8.1",
            "haddock/ghc-bignum-1.3",
            "haddock/ghc-prim-0.10.0",
            "haddock/index",
            "haddock/pretty-1.1.3.6",
            "haddock/template-haskell-2.20.0.0",
            "haddock/testsZShaddockZShaddock-lib-a",
            "haddock/testsZShaddockZShaddock-lib-b",
            "haddock/testsZShaddockZShaddock-lib-deep",
            "haddock/ghc-boot-th-9.6.5",
        ]
    elif test_ghc_version == "9.8.2" and is_nix_shell:
        return [
            "haddock/array-0.5.6.0-inplace",
            "haddock/base-4.19.1.0-inplace",
            "haddock/deepseq-1.5.0.0-inplace",
            "haddock/ghc-bignum-1.3-inplace",
            "haddock/ghc-prim-0.11.0-inplace",
            "haddock/index",
            "haddock/pretty-1.1.3.6-inplace",
            "haddock/template-haskell-2.21.0.0-inplace",
            "haddock/testsZShaddockZShaddock-lib-a",
            "haddock/testsZShaddockZShaddock-lib-b",
            "haddock/testsZShaddockZShaddock-lib-deep",
            "haddock/ghc-boot-th-9.8.2-inplace",
        ]
    elif test_ghc_version == "9.8.2" and is_windows:
        return [
            "haddock/array-0.5.6.0-eeeb",
            "haddock/base-4.19.0.0-1e7d",
            "haddock/deepseq-1.5.0.0-940f",
            "haddock/ghc-bignum-1.3-7ca5",
            "haddock/ghc-boot-th-9.8.2-d8a4",
            "haddock/ghc-prim-0.11.0-6ef2",
            "haddock/index",
            "haddock/pretty-1.1.3.6-39a4",
            "haddock/template-haskell-2.21.0.0-9348",
            "haddock/testsZShaddockZShaddock-lib-a",
            "haddock/testsZShaddockZShaddock-lib-b",
            "haddock/testsZShaddockZShaddock-lib-deep",
        ]
    elif test_ghc_version == "9.8.2" and is_linux:
        return [
            "haddock/array-0.5.6.0-7881",
            "haddock/base-4.19.1.0-179c",
            "haddock/deepseq-1.5.0.0-6708",
            "haddock/ghc-bignum-1.3-b9ac",
            "haddock/ghc-boot-th-9.8.2-9178",
            "haddock/ghc-prim-0.11.0-7523",
            "haddock/index",
            "haddock/pretty-1.1.3.6-8bb3",
            "haddock/template-haskell-2.21.0.0-9c7a",
            "haddock/testsZShaddockZShaddock-lib-a",
            "haddock/testsZShaddockZShaddock-lib-b",
            "haddock/testsZShaddockZShaddock-lib-deep",
        ]
    elif test_ghc_version == "9.8.2" and is_darwin:
        return [
            "haddock/array-0.5.6.0-a13d",
            "haddock/base-4.19.1.0-e86d",
            "haddock/deepseq-1.5.0.0-8f97",
            "haddock/ghc-bignum-1.3-625d",
            "haddock/ghc-boot-th-9.8.2-c69e",
            "haddock/ghc-prim-0.11.0-f42f",
            "haddock/index",
            "haddock/pretty-1.1.3.6-b285",
            "haddock/template-haskell-2.21.0.0-4c6f",
            "haddock/testsZShaddockZShaddock-lib-a",
            "haddock/testsZShaddockZShaddock-lib-b",
            "haddock/testsZShaddockZShaddock-lib-deep",
        ]
    else:
        fail("//tests:test-haddock is missing case : (test_ghc_version={}, cpu_value={})".format(test_ghc_version, cpu_value))
