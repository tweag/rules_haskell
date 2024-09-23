{-# LANGUAGE BangPatterns #-}

module Array.Traverse.Unsafe
  ( traversePoly
  , traverseMono
  ) where

import Control.Monad.ST
import Control.Monad.Trans.State.Strict
import Control.Monad.Primitive
import Data.Primitive.Array

{-# INLINE traversePoly #-}
traversePoly
  :: PrimMonad m
  => (a -> m b)
  -> Array a
  -> m (Array b)
traversePoly f = \ !ary ->
  let
    !sz = sizeofArray ary
    go !i !mary
      | i == sz
      = unsafeFreezeArray mary
      | otherwise
      = do
          a <- indexArrayM ary i
          b <- f a
          writeArray mary i b
          go (i + 1) mary
  in do
    mary <- newArray sz badTraverseValue
    go 0 mary

badTraverseValue :: a
badTraverseValue = die "traversePoly" "bad indexing"
{-# NOINLINE badTraverseValue #-}

die :: String -> String -> a
die fun problem = error $ "Array.Traverse.Unsafe" ++ fun ++ ": " ++ problem

-- Included to make it easy to inspect GHC Core that results
-- from inlining traversePoly.
traverseMono :: 
     (Int -> StateT Word (ST s) Int)
  -> Array Int
  -> StateT Word (ST s) (Array Int)
traverseMono f x = traversePoly f x
