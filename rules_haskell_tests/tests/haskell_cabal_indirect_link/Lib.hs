{-# LANGUAGE ForeignFunctionInterface #-}

module Lib(add) where

foreign import ccall "add" add :: Int -> Int -> Int
