{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

foreign import ccall "add" add :: Int -> Int -> Int

main :: IO ()
main = print $ add  1 1
