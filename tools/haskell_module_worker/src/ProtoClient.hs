{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CApiFFI #-}
-- | Reading and writing of protobuf messages from handles
module ProtoClient
  ( ProtoClient
  , WorkRequest(..)
  , createProtoClient
  , readWorkRequest
  , writeWorkResponse
  , redirectStdoutToStderr
  ) where

import Control.Monad (forM, when)
import GHC.Foreign (peekCString, withCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr
import Foreign.Storable (peek)
import System.IO (utf8)

newtype ProtoClient = ProtoClient (Ptr ProtoClient)
data WorkRequest = WorkRequest
  { wrArgs :: [String]
  , wrVerbosity :: Int
  }

foreign import capi unsafe "tools/haskell_module_worker/cbits/protoclient.h redirectStdoutToStderr"
  c_redirectStdoutToStderr :: IO CInt

foreign import capi unsafe "tools/haskell_module_worker/cbits/protoclient.h createProtoClient"
  c_createProtoClient :: CInt -> CInt -> IO (Ptr ProtoClient)

foreign import capi safe "tools/haskell_module_worker/cbits/protoclient.h readWorkRequest"
  c_readWorkRequest
    :: Ptr ProtoClient -> Ptr (Ptr (Ptr CChar)) -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import capi safe "tools/haskell_module_worker/cbits/protoclient.h writeWorkResponse"
  c_writeWorkResponse :: Ptr ProtoClient -> CInt -> Ptr CChar -> IO CInt

redirectStdoutToStderr :: IO CInt
redirectStdoutToStderr = c_redirectStdoutToStderr

-- | Takes the file descriptor of a file to read messages from and the
-- file descriptor of a file to write messages to.
createProtoClient :: CInt -> CInt -> IO ProtoClient
createProtoClient fdIn fdOut = do
    ProtoClient <$> c_createProtoClient fdIn fdOut

-- | Reads a request from stdin.
readWorkRequest :: ProtoClient -> IO WorkRequest
readWorkRequest (ProtoClient pc) =
    alloca $ \pverbosity ->
    alloca $ \ppargs ->
    alloca $ \pnargs -> do
      rc <- c_readWorkRequest pc ppargs pnargs pverbosity
      if rc /= 0 then error "readWorkRequest failed"
      else do
        verbosity <- peek pverbosity
        nargs <- peek pnargs
        pargs <- peek ppargs
        cstrings <- peekArray (fromIntegral nargs) pargs
        args <- forM cstrings $ \cstr ->
          peekCString utf8 cstr <* free cstr
        free pargs
        return $ WorkRequest args (fromIntegral verbosity)

-- | Writes a response to whatever file given to 'createProtoClient',
-- then flushes it.
--
-- Takes the exit code and the output.
writeWorkResponse :: ProtoClient -> Int -> String -> IO ()
writeWorkResponse (ProtoClient pc) exitCode output = do
    rc <- withCString utf8 output $
            c_writeWorkResponse pc (fromIntegral exitCode)
    when (rc /= 0) $ error "writeWorkResponse failed"
