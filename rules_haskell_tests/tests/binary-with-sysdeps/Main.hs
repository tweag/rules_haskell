module Main where

import Foreign.Ptr
import Foreign.C.Types

foreign import ccall crc32 :: CLong -> Ptr () -> CInt -> IO ()

main = crc32 0 nullPtr 0
