{-# LANGUAGE ForeignFunctionInterface #-}

module HsLibIndirect where

foreign import ccall "direct" c_direct :: Int -> Int

indirect :: Int -> Int
indirect n = c_direct n + 1
