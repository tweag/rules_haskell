module LibB (add_one_hs) where

import LibA (add_one)
import Data.Int (Int32)

foreign export ccall add_one_hs :: Int32 -> IO Int32

add_one_hs :: Int32 -> IO Int32
add_one_hs x = pure $! add_one x + 0
