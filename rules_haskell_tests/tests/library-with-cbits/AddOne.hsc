module AddOne where

foreign import ccall "c_add_one" addOne :: Int -> Int
