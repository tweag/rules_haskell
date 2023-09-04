{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import qualified HsLib

foreign import ccall "value" value :: Int

main = print $ HsLib.value + value
