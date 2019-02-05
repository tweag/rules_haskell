{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module HsLibIndirect (indirect) where

import HsLibIndirectTH

foreign import ccall "direct" c_direct :: Int -> Int

indirect :: Int -> Int
indirect n = c_direct n + $(one)
