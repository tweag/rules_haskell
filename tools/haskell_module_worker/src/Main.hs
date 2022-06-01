{-# LANGUAGE LambdaCase #-}
module Main where

import Compile (Status(..), runSession, compile)
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.List (intersperse)
import Data.Word (Word64)
import GHC.Stats (getRTSStats, getRTSStatsEnabled, max_live_bytes)
import Options (Options(..), parseArgs)
import ProtoClient
  ( WorkRequest(..)
  , createProtoClient
  , readWorkRequest
  , writeWorkResponse
--  , redirectStdoutToStderr
  )
import System.Clock
import System.Environment (getArgs)
import System.IO (IOMode(WriteMode), hFlush, hPrint, hPutStrLn, hSetBinaryMode, stdin, stderr, stdout, withFile)
import System.Posix.Process (getProcessID)
import qualified Worker as Worker (Handle, interact, withWorker)


main :: IO ()
main = do
    opts <- getArgs >>= parseArgs
    hSetBinaryMode stdin True
    hSetBinaryMode stdout True

    case optServerExecutable opts of
      Just exePath ->
        Worker.withWorker exePath (forwardRequests opts)
      Nothing ->
        workerLoop
        -- when (optPersist opts) $
        --  terminateIfUsingTooMuchMemory (optMemoryAllowance opts)

-- | Sends requests to the worker process
forwardRequests :: Options -> Worker.Handle -> IO ()
forwardRequests opts wh = do
    let stdout_dup = 1
    -- stdout_dup <- redirectStdoutToStderr
    pc <- createProtoClient 0 stdout_dup
    pid <- getProcessID
    withFile ("/tmp/persistenworker_" ++ show pid) WriteMode $ \h ->
      (if optPersist opts then forever else id) $ do
        -- timerThreadId <- startTimerThread (2 * 1000000) (resetWorkerLoop whMV)
        wr <- readWorkRequest pc
        t0 <- getTime Monotonic
        st <- Worker.interact wh wr
        tf <- getTime Monotonic
        let d = tf - t0
        hPrint h (fromIntegral (sec d) + (fromIntegral (nsec d) / 1000000000) :: Double)
        writeWorkResponse pc (statusExitCode st) (statusOutput st)
  where
    statusExitCode = \case { Succeeded{} -> 0; _ -> 1 }
    statusOutput = \case
      Succeeded logs -> unlines $ intersperse "" logs
      CompileErrors logs errs -> unlines $ intersperse "" $ logs ++ errs
      NonHaskellInputs files ->
        unlines $ "haskell_module_worker error: non-haskell inputs:" : files
      NonOneShotCompilation ->
        "haskell_module_worker error: only on-shot compilation (-c) is supported"

workerLoop :: IO ()
workerLoop = do
    inputBS0 <- ByteString.Lazy.getContents
    runSession $ go inputBS0
  where
    go bs = do
      case Binary.decodeOrFail bs of
        Right (rest, _, wr) -> do
          st <- compile (wrArgs wr) (wrVerbosity wr)
          liftIO $ do
            ByteString.Lazy.putStr (Binary.encode st)
            hFlush stdout
          go rest
        Left (_, _, err) ->
          liftIO $ hPutStrLn stderr $ "Error decoding work request: " ++ err

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
