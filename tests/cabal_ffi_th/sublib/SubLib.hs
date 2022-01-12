{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : SubLib
Description : Test compilation of cabal sublibrary
-}

module SubLib where

import Foreign.C.Types (CInt(..))

foreign import ccall "c_add_one"
  c_add_one :: CInt -> CInt

ten :: Int
ten = fromIntegral (c_add_one 9)

subLibVal = "SubLib " ++ show $([| ten |])
