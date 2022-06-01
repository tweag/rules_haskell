-- | An interface to do RPC with external processses
--
-- Processes can be started, they can answer requests,
-- and they can be terminated.
module Worker
  ( Handle
  , interact
  , withWorker
  ) where

import Control.Concurrent.MVar
import Control.Exception (bracket)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Prelude hiding (interact)
import System.IO (hFlush, hSetBinaryMode)
import qualified System.IO as IO (Handle)
import System.Process
  ( CreateProcess(..)
  , ProcessHandle
  , StdStream(..)
  , createProcess
  , proc
  , terminateProcess
  , waitForProcess
  )


-- | A worker is a thread that runs the compiler on a loop.
newtype Handle = Handle (MVar Info)

-- | Handles for communicating with the worker, the process
-- identifier that it is used to kill it, and the output
-- in lazy-bytestring IO form.
data Info = Info
    { wiProcessHandle :: ProcessHandle
    , wiIn :: IO.Handle
    , wiOut :: IO.Handle
    , wiOutBS :: ByteString.Lazy.ByteString
    }

-- | Starts a worker, performs the given action, and terminates the worker.
-- Waits until the worker finishes.
withWorker :: FilePath -> (Handle -> IO a) -> IO a
withWorker exePath =
    bracket
      (initWorker exePath >>= fmap Handle . newMVar)
      kill

-- | Send a request @a@ and expect a response @b@.
--
-- May throw an exception if the worker dies.
interact :: (Binary a, Binary b) => Handle -> a -> IO b
interact (Handle mv) a =
    modifyMVar mv $ \wi -> do
      ByteString.Lazy.hPutStr (wiIn wi) (Binary.encode a)
      hFlush (wiIn wi)
      case Binary.decodeOrFail (wiOutBS wi) of
        Right (rest, _, b) -> return (wi { wiOutBS = rest }, b)
        Left (_, _, err) -> error $ "Error decoding status: " ++ err

kill :: Handle -> IO ()
kill (Handle mv) =
    withMVar mv $ \wi -> do
      terminateProcess (wiProcessHandle wi)
      _ <- waitForProcess (wiProcessHandle wi)
      return ()

initWorker :: FilePath -> IO Info
initWorker exePath = do
    (mpin, mpout, _merr, ph) <- createProcess (proc exePath [])
      { std_in = CreatePipe
      , std_out = CreatePipe
      }
    case (mpin, mpout) of
      (Just pin, Just pout) -> do
        hSetBinaryMode pin True
        hSetBinaryMode pout True
        outBS <- ByteString.Lazy.hGetContents pout
        return Info
          { wiProcessHandle = ph
          , wiIn = pin
          , wiOut = pout
          , wiOutBS = outBS
          }
      _ -> error "error starting worker"
