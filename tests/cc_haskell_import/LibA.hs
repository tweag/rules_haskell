module LibA (add_one) where

import Data.Int (Int32)

foreign import ccall "c_add_one" c_add_one' :: Int32 -> Int32

add_one :: Int32 -> Int32
add_one x = c_add_one' x
