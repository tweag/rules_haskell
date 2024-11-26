{-# OPTIONS -Wall #-}

import Data.List (isInfixOf)
import IntegrationTesting
import Test.Hspec (describe, hspec, it)

main :: IO ()
main =  hspec $ do
  describe "bazel run repl" $ do
    it "fails with a json decode error" $ do
      -- this is evidence that it used the deficient `stack` script provided in hs_override_stack_test/
      bazel <- setupTestBazel
      let p (_stdout, stderr) = "Error in decode: at offset" `isInfixOf` stderr
       in
         failedOutputSatisfy p (bazel ["run", "//:hs-bin@repl", "--", "-ignore-dot-ghci", "-e", ":main"])
