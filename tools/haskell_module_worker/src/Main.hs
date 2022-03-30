module Main where

import Compile (runSession, compile)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word64)
import GHC.Stats (getRTSStats, getRTSStatsEnabled, max_live_bytes)
import Options (parseArgs)
import System.Environment (getArgs)

main :: IO ()
main = do
    (args, memoryAllowance) <- getArgs >>= parseArgs
    st <- runSession $ do
      st <- compile args 0
      liftIO $ terminateIfUsingTooMuchMemory memoryAllowance
      return st
    print st

-- | Terminates the worker if it exceeds the memory allowance
terminateIfUsingTooMuchMemory :: Word64 -> IO ()
terminateIfUsingTooMuchMemory memoryAllowance = do
    statsEnabled <- getRTSStatsEnabled
    unless statsEnabled $
      error "terminateIfUsingTooMuchMemory: worker built without -with-rtsopts=-T"
    stats <- getRTSStats
    when (max_live_bytes stats > memoryAllowance * 1024 * 1024) $
      error $
        "terminateIfUsingTooMuchMemory: worker reached the memory threshold of "
        ++ show memoryAllowance ++ " MB"
