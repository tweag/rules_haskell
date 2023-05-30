module Lib (crc) where

import Foreign.Ptr
import Foreign.C.Types

foreign import ccall crc32 :: CLong -> Ptr () -> CInt -> IO ()

crc = crc32 0 nullPtr 0
