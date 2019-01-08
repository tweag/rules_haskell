{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Word ( Word8)
import Foreign.C.Types
import Foreign.Ptr (Ptr)

-- | Decompress a bytestring.
foreign import ccall unsafe "LZ4_decompress_safe"
  c_LZ4_decompress_safe :: Ptr CChar -- ^ source
                        -> Ptr Word8 -- ^ destination
                        -> CInt      -- ^ source size
                        -> CInt      -- ^ max decompressed size
                        -> IO CInt   -- ^ decompression result

main = do
  print =<< c_LZ4_decompress_safe undefined undefined undefined undefined
