module AddOne where

import Control.Monad.Trans.Except

foreign import ccall "c_add_one" addOne :: Int -> Int

foo = runExceptT
