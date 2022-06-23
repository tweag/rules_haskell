{-# OPTIONS -Wall #-}

import IntegrationTesting

main :: IO ()
main =  do
    bazel <- setupTestBazel
    let p (stdout, _stderr) = lines stdout == ["Hello GHCi!"]
     in
        outputSatisfy p (bazel ["run", "//:hs-bin@repl", "--", "-ignore-dot-ghci", "-e", ":main"])
