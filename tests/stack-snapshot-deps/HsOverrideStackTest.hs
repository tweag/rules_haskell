{-# OPTIONS -Wall #-}

import Data.List (isInfixOf)
import IntegrationTesting

main :: IO ()
main =  do
    bazel <- setupTestBazel
    let p (_stdout, stderr) = "parsing JSON failed" `isInfixOf` stderr
     in
        failedOutputSatisfy p (bazel ["run", "//:hs-bin@repl", "--", "-ignore-dot-ghci", "-e", ":main"])
