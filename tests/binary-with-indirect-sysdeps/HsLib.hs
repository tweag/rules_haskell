module HsLib where

import Foreign.Ptr
import Foreign.C.Types

foreign import ccall crc32 :: CLong -> Ptr () -> CInt -> IO ()

test = crc32 0 nullPtr 0
