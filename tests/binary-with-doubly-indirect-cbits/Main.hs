{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

foreign import ccall "value" value :: Int

main = print value
