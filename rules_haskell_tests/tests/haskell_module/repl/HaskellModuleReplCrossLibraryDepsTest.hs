{-# OPTIONS -Wall #-}

import Test.Hspec (hspec, it)
import IntegrationTesting

main :: IO ()
main =  hspec $ do
  it "bazel run repl" $ do
    bazel <- setupTestBazel
    let p (stdout, _stderr) = lines stdout == ["42"]
     in
       outputSatisfy p (bazel ["run", "//package-b:package-b@repl", "--", "-ignore-dot-ghci", "-e", "mod1num"])
