{-# LANGUAGE LambdaCase #-}
module Main where

import Compile (Status(..), runSession, compile)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception (SomeException, bracketOnError, finally, handle, throwIO)
import Control.Monad (forM_, forever, join, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.IORef
import Data.List (intersperse, isPrefixOf)
import qualified Data.Text.IO as Text
import Data.Word (Word64)
import GHC.Fingerprint (Fingerprint, fingerprintFingerprints, fingerprintString)
import GHC.Stats (getRTSStats, getRTSStatsEnabled, max_live_bytes)
import LRUCache
import Options (Options(..), parseArgs)
import ProtoClient
  ( ProtoClient
  , WorkRequest(..)
  , createProtoClient
  , readWorkRequest
  , writeWorkResponse
--  , redirectStdoutToStderr
  )
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>), takeDirectory)
import System.Environment (getArgs)
import System.IO
  ( BufferMode(LineBuffering)
  , hFlush
  , hPutStr
  , hPutStrLn
  , hSetBinaryMode
  , hSetBuffering
  , stdin
  , stderr
  , stdout
  )
import qualified Worker as Worker (Handle, interact, kill, new)


main :: IO ()
main = do
    opts <- getArgs >>= parseArgs
    hSetBinaryMode stdin True
    hSetBinaryMode stdout True
    hSetBuffering stderr LineBuffering

    case optServerExecutable opts of
      Just exePath -> forwardRequests exePath opts
      Nothing -> workerLoop

data Worker = Worker
  { wHandle :: Worker.Handle
  , wIdle :: IORef Bool
  , wId :: Int
  }

type Cache = MVar (LRUCache Fingerprint Worker)


-- | Sends requests to the worker processes
forwardRequests :: FilePath -> Options -> IO ()
forwardRequests exePath opts = do
    let stdout_dup = 1
    -- stdout_dup <- redirectStdoutToStderr
    pc <- createProtoClient 0 stdout_dup
    idGen <- newIORef 0
    outChan <- newChan
    _ <- forkIO $ forever $ join $ readChan outChan
    lruV <- newMVar LRUCache
          { lruCapacity = optNumWorkers opts
          , lruElements = []
          , lruAlloc = allocWorker idGen outChan
          , lruCleanup = cleanupWorker
          }
    semWorkers <- newQSem (fromIntegral $ optNumWorkers opts)

    serveRequests opts semWorkers outChan lruV pc
      `finally` do
        lru <- takeMVar lruV
        void $ LRUCache.discard (const True) lru
  where
    allocWorker idGen outChan k = do
      idleRef <- newIORef True
      i <- readIORef idGen
      writeIORef idGen (i + 1)
      hPutStrLn stderr $ "Starting background worker (id " ++ show i ++ ") with key: " ++ show k
      wh <- Worker.new exePath [] $ \msg ->
        writeChan outChan $ do
          hPutStr stderr $ "worker " ++ show i ++ ": "
          Text.hPutStrLn stderr msg
      return Worker
        { wHandle = wh
        , wIdle = idleRef
        , wId = i
        }

    cleanupWorker w = do
      hPutStrLn stderr $ "Terminating background worker (id " ++ show (wId w) ++ ")"
      Worker.kill (wHandle w)

serveRequests :: Options -> QSem -> Chan (IO ()) -> Cache -> ProtoClient -> IO ()
serveRequests opts semWorkers outChan lruV pc = go
  where
    go = do
      forwardRequest semWorkers outChan lruV pc
      if optPersist opts then
        go
      else
        return ()

forwardRequest :: QSem -> Chan (IO ()) -> Cache -> ProtoClient -> IO ()
forwardRequest semWorkers outChan lruV pc = do
    waitQSem semWorkers
    wr <- readWorkRequest pc
    k <- requestKey (wrSandboxDir wr) wr

    void $ forkIO $ (`finally` signalQSem semWorkers) $ do
      bracketOnError (getWorker k) dropWorker
        $ \w -> do
          st <- handle (reportFailure w wr) $ (`finally` writeIORef (wIdle w) True) $ do
            writeIORef (wIdle w) False
            let wh = wHandle w
            Worker.interact wh wr
          writeChan outChan $
            writeWorkResponse pc (wrRequestId wr) (statusExitCode st) (statusOutput st)
  where
    reportFailure w wr e = do
      writeChan outChan $
        writeWorkResponse pc (wrRequestId wr) 1 $
          "Communication with background worker " ++ show (wId w) ++ " failed: " ++ show (e :: SomeException)
      throwIO e

    getWorker k =
      modifyMVar lruV $ \lru -> do
        (lru', w) <- LRUCache.get k (readIORef . wIdle) lru
        writeIORef (wIdle w) False
        return (lru', w)

    dropWorker w = modifyMVar_ lruV $ LRUCache.discard ((wId w ==) . wId)

    statusExitCode = \case { Succeeded{} -> 0; _ -> 1 }

    statusOutput = \case
      Succeeded logs -> unlines $ intersperse "" logs
      CompileErrors logs errs -> unlines $ intersperse "" $ logs ++ errs
      NonHaskellInputs files ->
        unlines $ "haskell_module_worker error: non-haskell inputs:" : files
      NonOneShotCompilation ->
        "haskell_module_worker error: only on-shot compilation (-c) is supported"

    requestKey msandboxDir wr = do
      dbs <- readPackageEnv msandboxDir (wrArgs wr)
      let dbs2 = collectPackageDbs (wrArgs wr)
          odir = collectODir (wrArgs wr)
      return $ fingerprintFingerprints $ map fingerprintString (odir ++ dbs ++ dbs2)

readPackageEnv :: Maybe FilePath -> [String] -> IO [String]
readPackageEnv msandboxDir args =
    case drop 1 $ dropWhile ("-package-env" /=) args of
      envFile : _ -> do
        s <- readFile $ maybe envFile (</> envFile) msandboxDir
        return $ takeDirectory envFile : [ x | x <- lines s, "package-db" `isPrefixOf` x ]
      [] ->
        return []

collectODir :: [String] -> [String]
collectODir ("-odir" : x : _) = [x]
collectODir (_ : xs) = collectODir xs
collectODir [] = []

collectPackageDbs :: [String] -> [String]
collectPackageDbs ("-package-db" : x : xs) = x : collectPackageDbs xs
collectPackageDbs (_ : xs) = collectPackageDbs xs
collectPackageDbs [] = []

workerLoop :: IO ()
workerLoop = do
    inputBS0 <- ByteString.Lazy.getContents
    cwd <- getCurrentDirectory
    runSession $ go cwd inputBS0
  where
    go cwd bs = do
      case Binary.decodeOrFail bs of
        Right (rest, _, wr) -> do
          liftIO $ forM_ (wrSandboxDir wr) (setCurrentDirectory . (cwd </>))
          st <- compile (wrArgs wr) (wrVerbosity wr)
          liftIO $ do
            ByteString.Lazy.putStr (Binary.encode st)
            hFlush stdout
          go cwd rest
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
