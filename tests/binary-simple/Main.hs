{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

foreign import ccall "c_add_one" addOne :: Int -> Int

main = print $ addOne 41
