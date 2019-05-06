{-# LANGUAGE ForeignFunctionInterface #-}

module Lib (three) where

foreign import ccall "one" one :: Int
foreign import ccall "two" two :: Int

three :: Int
three = one + two
