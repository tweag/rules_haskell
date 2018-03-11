{-# LANGUAGE ForeignFunctionInterface #-}

module Foo (foo) where

foreign import ccall "add_seven"
  c_add_seven :: Int -> Int

foo :: Int -> Int
foo = (+ 5) . c_add_seven
