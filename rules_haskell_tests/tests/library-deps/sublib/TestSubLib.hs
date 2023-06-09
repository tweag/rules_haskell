{-# LANGUAGE ForeignFunctionInterface #-}
module TestSubLib (messageEnd) where

messageEnd :: String
messageEnd = "world " ++ show (foo 10)

foreign import ccall foo :: Int -> Int
