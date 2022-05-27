-- | An interface to do RPC with external processses
--
-- Processes can be started, they can answer requests,
-- and they can be terminated.
module Worker
  ( Handle
  , interact
  , new
  , kill
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Text (Text)
import qualified Data.Text.IO as Text
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

new
  :: FilePath         -- ^ Executable to launch
  -> [String]         -- ^ Arguments for the executable
  -> (Text -> IO ())  -- ^ Action invoked on every line from the
                      --   stderr of the worker.
  -> IO Handle
new exePath args handleStderrLine =
    initWorker exePath args handleStderrLine >>= fmap Handle . newMVar

-- | Terminates the worker and waits until it finishes.
kill :: Handle -> IO ()
kill (Handle mv) =
    withMVar mv $ \wi -> do
      terminateProcess (wiProcessHandle wi)
      _ <- waitForProcess (wiProcessHandle wi)
      return ()

initWorker
  :: FilePath
  -> [String]
  -> (Text -> IO ())
  -> IO Info
initWorker exePath args handleStderrLine = do
    (mpin, mpout, mperr, ph) <- createProcess (proc exePath args)
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }
    case (mpin, mpout, mperr) of
      (Just pin, Just pout, Just perr) -> do
        hSetBinaryMode pin True
        hSetBinaryMode pout True
        outBS <- ByteString.Lazy.hGetContents pout
        _ <- forkIO $ forever $
          Text.hGetLine perr >>= handleStderrLine
        return Info
          { wiProcessHandle = ph
          , wiIn = pin
          , wiOut = pout
          , wiOutBS = outBS
          }
      _ -> error "error starting worker"
