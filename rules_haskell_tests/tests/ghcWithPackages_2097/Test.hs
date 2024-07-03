{-# OPTIONS -Wall #-}

import Test.Hspec (hspec, it)
import IntegrationTesting

main :: IO ()
main =  hspec $ do
  it "bazel run ghcWithPackages" $ do
    bazel <- setupTestBazel
    assertSuccess (bazel ["run", "-s", "--verbose_failures", "--noenable_bzlmod", "//:add-one"])
    -- assertSuccess (bazel ["cquery", "--output", "build", "@rules_haskell_ghc_nixpkgs_haskell_toolchain//:rts"])
