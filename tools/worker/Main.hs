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

    -- redirect stdout to stderr
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
