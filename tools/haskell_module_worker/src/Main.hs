{-# LANGUAGE LambdaCase #-}
module Main where

import Compile (Status(..), runSession, compile)
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import Data.Word (Word64)
import GHC.Stats (getRTSStats, getRTSStatsEnabled, max_live_bytes)
import Options (Options(..), parseArgs)
import ProtoClient
  ( WorkRequest(..)
  , createProtoClient
  , readWorkRequest
  , writeWorkResponse
  , redirectStdoutToStderr
  )
import System.Clock
import System.Environment (getArgs)
import System.IO (IOMode(WriteMode), hPrint, hSetBinaryMode, stdin, stdout, withFile)
import System.Posix.Process (getProcessID)

main :: IO ()
main = do
    opts <- getArgs >>= parseArgs
    hSetBinaryMode stdin True
    hSetBinaryMode stdout True
    stdout_dup <- redirectStdoutToStderr
    pc <- createProtoClient 0 stdout_dup
    pid <- getProcessID

    withFile ("/tmp/persistenworker_" ++ show pid) WriteMode $ \h -> runSession $ (if optPersist opts then forever else id) $ do
      wr <- liftIO $ readWorkRequest pc
      t0 <- liftIO $ getTime Monotonic
      st <- compile (wrArgs wr) (wrVerbosity wr)
      tf <- liftIO $ getTime Monotonic
      let d = tf - t0
      liftIO $ hPrint h (fromIntegral (sec d) + (fromIntegral (nsec d) / 1000000000) :: Double)
      liftIO $ writeWorkResponse pc (statusExitCode st) (statusOutput st)
      liftIO $ when (optPersist opts) $
        terminateIfUsingTooMuchMemory (optMemoryAllowance opts)
  where
    statusExitCode = \case { Succeeded{} -> 0; _ -> 1 }
    statusOutput = \case
      Succeeded logs -> unlines $ intersperse "" logs
      CompileErrors logs errs -> unlines $ intersperse "" $ logs ++ errs
      NonHaskellInputs files ->
        unlines $ "haskell_module_worker error: non-haskell inputs:" : files
      NonOneShotCompilation ->
        "haskell_module_worker error: only on-shot compilation (-c) is supported"

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
