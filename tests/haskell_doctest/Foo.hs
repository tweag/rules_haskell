{-# LANGUAGE ForeignFunctionInterface #-}

module Foo (foo) where

import Foreign.C.Types (CInt(..))

foreign import ccall "add_five"
  c_add_five :: CInt -> CInt

-- |
-- >>> foo
-- 5

foo :: Int
foo = fromIntegral (c_add_five 0)
