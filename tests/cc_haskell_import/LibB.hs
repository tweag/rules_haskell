module LibB (add_one_hs) where

import LibA (add_one)
import Foreign.C.Types (CInt(..))

foreign export ccall add_one_hs :: Int -> IO Int

add_one_hs :: Int -> IO Int
add_one_hs x = pure $! add_one x + 0
