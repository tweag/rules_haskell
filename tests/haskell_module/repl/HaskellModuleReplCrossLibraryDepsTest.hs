{-# OPTIONS -Wall #-}

import IntegrationTesting

main :: IO ()
main =  do
    bazel <- setupTestBazel
    let p (stdout, _stderr) = lines stdout == ["42"]
     in
        outputSatisfy p (bazel ["run", "//package-b:package-b@repl", "--", "-ignore-dot-ghci", "-e", "mod1num"])
