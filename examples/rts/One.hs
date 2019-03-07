module One () where

add_one_hs :: Int -> Int
add_one_hs x = x + 1

foreign export ccall add_one_hs :: Int -> Int
