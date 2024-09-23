{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}

module Array.Traverse.Closure
  ( traversePoly
  ) where

import Control.Applicative
import Control.Monad.ST
import Data.Primitive.Array
import GHC.Exts (Int(..),MutableArray#)

{-# INLINE traversePoly #-}
traversePoly
  :: Applicative f
  => (a -> f b)
  -> Array a
  -> f (Array b)
traversePoly f = \ !ary ->
  let
    !len = sizeofArray ary
    go !i
      | i == len = pure $ STA $ \mary -> unsafeFreezeArray (MutableArray mary)
      | (# x #) <- indexArray## ary i
      = liftA2 (\b (STA m) -> STA $ \mary ->
                  writeArray (MutableArray mary) i b >> m mary)
               (f x) (go (i + 1))
  in if len == 0
     then pure mempty
     else runSTA len <$> go 0

badTraverseValue :: a
badTraverseValue = die "traversePoly" "bad indexing"
{-# NOINLINE badTraverseValue #-}

die :: String -> String -> a
die fun problem = error $ "Array.Traverse.Closure" ++ fun ++ ": " ++ problem

newtype STA a = STA {_runSTA :: forall s. MutableArray# s a -> ST s (Array a)}

runSTA :: Int -> STA a -> Array a
runSTA !sz = \ (STA m) -> runST $ newArray_ sz >>= \ ar -> m (marray# ar)
{-# INLINE runSTA #-}

newArray_ :: Int -> ST s (MutableArray s a)
newArray_ !n = newArray n badTraverseValue

