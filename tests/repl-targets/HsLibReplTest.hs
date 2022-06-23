{-# OPTIONS -Wall #-}

import IntegrationTesting

main :: IO ()
main =  do
    bazel <- setupTestBazel
    let p (stdout, _stderr) = lines stdout == ["\"16barbazgen\""]
     in
        outputSatisfy p (bazel ["run", "//:hs-lib@repl", "--", "-ignore-dot-ghci", "-e", "show (foo 10) ++ bar ++ baz ++ gen"])
