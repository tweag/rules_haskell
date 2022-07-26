{-# LANGUAGE ForeignFunctionInterface #-}

module Foo (foo) where

foreign import ccall "c_add_one"
  c_add_one :: Int -> Int

foo :: Int -> Int
foo = (+ 5) . c_add_one
