module CLib (value) where

foreign import ccall "value" value :: Int
