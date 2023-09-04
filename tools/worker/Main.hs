module Main where

import Server (server)

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

--import GHC.IO.Handle (hDuplicate, hDuplicateTo)

pwFlag :: String
pwFlag = "--persistent_worker"

main :: IO ()
main = do

    -- Redirecting stdout to stderr trick is, albeit convenient, fragile under
    -- heavy parallelism https://gitlab.haskell.org/ghc/ghc/issues/16819
    -- it fails, e.g., when Bazel spawns multiple workers while running
    -- the test suite; it's left here because may be useful for debugging
    -- stdout_dup <- hDuplicate stdout
    -- hDuplicateTo stderr stdout
    let stdout_dup = stdout

    args <- getArgs
    hPutStrLn stderr $ "Args taken: " ++ show args
    if pwFlag `elem` args
      then server stdin stdout_dup $ filter (/= pwFlag) args
      else
          print "Worker should be called with --persistent_worker"
          >> exitFailure
