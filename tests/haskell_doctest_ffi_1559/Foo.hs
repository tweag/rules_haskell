{-# LANGUAGE ForeignFunctionInterface #-}

module Foo (foo) where

import Foreign.C.Types (CInt(..))

foreign import ccall "c_add_one"
  c_add_one :: CInt -> CInt

-- |
-- >>> foo
-- 5
foo :: Int
foo = fromIntegral (c_add_one 4)
