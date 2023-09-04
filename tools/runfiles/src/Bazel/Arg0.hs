module Bazel.Arg0 (getArg0) where

import Foreign
import Foreign.C
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (argvEncoding)

foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

-- | Return the zeroth argument @argv[0]@.
--
-- In contrast to 'System.Environment.getProgName' this returns the full
-- zeroth argument, not just the basename.
--
-- In contrast to 'System.Environment.getExecutablePath' this does not
-- resolve symbolic links or lookup @/proc/self/exe@.
--
-- The implementation is a slight modification of the implementation of
-- 'System.Environment.getProgName'.
getArg0 :: IO FilePath
getArg0 =
  alloca $ \p_argc ->
    alloca $ \p_argv -> do
      getProgArgv p_argc p_argv
      argv <- peek p_argv
      unpackProgName argv

unpackProgName :: Ptr (Ptr CChar) -> IO String
unpackProgName argv = do
  enc <- argvEncoding
  peekElemOff argv 0 >>= GHC.peekCString enc
