{-# OPTIONS -Wall #-}

import Test.Hspec (hspec, it)
import IntegrationTesting

main :: IO ()
main =  hspec $ do
  it "bazel run repl" $ do
    bazel <- setupTestBazel
    let p (stdout, _stderr) = lines stdout == ["Hello GHCi!"]
     in
       outputSatisfy p (bazel ["run", "//:hs-bin@repl", "--", "-ignore-dot-ghci", "-e", ":main"])
