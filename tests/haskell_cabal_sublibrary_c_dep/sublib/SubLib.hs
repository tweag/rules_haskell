{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : SubLib
Description : Test compilation of cabal sublibrary
-}
module SubLib where

import Text.PrettyPrint.Annotated.Leijen
import Foreign.C.Types (CInt(..))

foreign import ccall "c_add_one"
  c_add_one :: CInt -> CInt

ten :: Int
ten = fromIntegral (c_add_one 9)

subLibVal = "SubLib " ++ show ten
