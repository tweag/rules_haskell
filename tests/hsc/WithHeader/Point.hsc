#include "point.h"

{-# LANGUAGE MagicHash #-}

module WithHeader.Point where

import Foreign.Ptr
import Foreign.Storable
import Data.Int

data Point = MkPoint {x :: Int32, y :: Int32}
  deriving Show

instance Storable Point where
  sizeOf _ = #{size point}
  alignment _ = #{alignment point}
  poke ptr (MkPoint x y) = do
    #{poke point, x} ptr x
    #{poke point, y} ptr y
  peek ptr = do
    x <- #{peek point, x} ptr
    y <- #{peek point, y} ptr
    pure $ MkPoint x y
