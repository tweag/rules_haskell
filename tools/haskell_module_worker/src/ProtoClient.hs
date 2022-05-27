{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CApiFFI #-}
-- | Reading and writing of protobuf messages from handles
module ProtoClient
  ( ProtoClient
  , WorkRequest(..)
  , createProtoClient
  , readWorkRequest
  , writeWorkResponse
  ) where

import Control.Monad (forM, when)
import Data.Binary (Binary)
import GHC.Foreign (peekCString, withCString)
import GHC.Generics (Generic)
import GHC.IO.Handle.FD (handleToFd)
import GHC.IO.FD (fdFD)
import Foreign.C.String (CString)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr
import Foreign.Storable (peek)
import System.IO (Handle, utf8)

newtype ProtoClient = ProtoClient (Ptr ProtoClient)
data WorkRequest = WorkRequest
  { wrRequestId :: Int
  , wrArgs :: [String]
  , wrVerbosity :: Int
  , wrSandboxDir :: Maybe FilePath
  }
  deriving Generic

instance Binary WorkRequest

foreign import capi unsafe "tools/haskell_module_worker/cbits/protoclient.h createProtoClient"
  c_createProtoClient :: CInt -> CInt -> IO (Ptr ProtoClient)

foreign import capi safe "tools/haskell_module_worker/cbits/protoclient.h readWorkRequest"
  c_readWorkRequest
    :: Ptr ProtoClient -> Ptr CInt -> Ptr (Ptr (Ptr CChar)) -> Ptr CInt -> Ptr CInt -> Ptr CString -> IO CInt

foreign import capi safe "tools/haskell_module_worker/cbits/protoclient.h writeWorkResponse"
  c_writeWorkResponse :: Ptr ProtoClient -> CInt -> CInt -> Ptr CChar -> IO CInt

-- | Takes the handle of file to read messages from and the
-- handle of a file to write messages to.
createProtoClient :: Handle -> Handle -> IO ProtoClient
createProtoClient hIn hOut = do
    fdIn <- handleToFd hIn
    fdOut <- handleToFd hOut
    ProtoClient <$> c_createProtoClient (fdFD fdIn) (fdFD fdOut)

-- | Reads a request from stdin.
readWorkRequest :: ProtoClient -> IO WorkRequest
readWorkRequest (ProtoClient pc) =
    alloca $ \prequestId ->
    alloca $ \pverbosity ->
    alloca $ \ppargs ->
    alloca $ \pnargs ->
    alloca $ \psandboxDir -> do
      rc <- c_readWorkRequest pc prequestId ppargs pnargs pverbosity psandboxDir
      if rc /= 0 then error "readWorkRequest failed"
      else do
        requestId <- peek prequestId
        verbosity <- peek pverbosity
        nargs <- peek pnargs
        pargs <- peek ppargs
        cstrings <- peekArray (fromIntegral nargs) pargs
        args <- forM cstrings $ \cstr ->
          peekCString utf8 cstr <* free cstr
        free pargs
        csandboxDir <- peek psandboxDir
        sandboxDir <- peekCString utf8 csandboxDir <* free csandboxDir
        return $ WorkRequest
          { wrRequestId = fromIntegral requestId
          , wrArgs = args
          , wrVerbosity = fromIntegral verbosity
          , wrSandboxDir = if null sandboxDir then Nothing else Just sandboxDir
          }

-- | Writes a response to whatever file given to 'createProtoClient',
-- then flushes it.
--
-- Takes the exit code and the output.
writeWorkResponse :: ProtoClient -> Int -> Int -> String -> IO ()
writeWorkResponse (ProtoClient pc) requestId exitCode output = do
    rc <- withCString utf8 output $
            c_writeWorkResponse pc (fromIntegral requestId) (fromIntegral exitCode)
    when (rc /= 0) $ error "writeWorkResponse failed"
