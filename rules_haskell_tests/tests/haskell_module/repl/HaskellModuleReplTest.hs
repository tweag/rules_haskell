{-# OPTIONS -Wall #-}

import Test.Hspec (hspec, it)
import IntegrationTesting

main :: IO ()
main =  hspec $ do
  it "bazel run repl" $ do
    bazel <- setupTestBazel
    let p (stdout, _stderr) = lines stdout == ["420"]
     in
      outputSatisfy p (bazel ["run", "//:repl", "--", "-ignore-dot-ghci", "-e", "leaf"])
