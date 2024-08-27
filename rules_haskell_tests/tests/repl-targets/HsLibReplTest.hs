{-# OPTIONS -Wall #-}

import Test.Hspec (hspec, it)
import IntegrationTesting

main :: IO ()
main =  hspec $ do
  it "bazel run repl" $ do
    bazel <- setupTestBazel
    let p (stdout, _stderr) = lines stdout == ["\"16barbazgen\""]
     in
       outputSatisfy p (bazel ["run", "//:hs-lib@repl", "-s", "--", "-ignore-dot-ghci", "-e", "show (foo 10) ++ bar ++ baz ++ gen"])
