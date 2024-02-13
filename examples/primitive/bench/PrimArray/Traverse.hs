module PrimArray.Traverse
  ( benchmarkApplicative
  , benchmarkPrimMonad
  , argument
  ) where

import Control.Monad.ST (runST)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bool (bool)
import Data.Primitive.PrimArray
import GHC.Exts (fromList)

benchmarkApplicative :: PrimArray Int -> Maybe (PrimArray Int)
benchmarkApplicative xs = traversePrimArray (\x -> bool Nothing (Just (x + 1)) (x > 0)) xs

benchmarkPrimMonad :: PrimArray Int -> Maybe (PrimArray Int)
benchmarkPrimMonad xs = runST $ runMaybeT $ traversePrimArrayP
  (\x -> bool (MaybeT (return Nothing)) (MaybeT (return (Just (x + 1)))) (x > 0))
  xs

argument :: PrimArray Int
argument = fromList (enumFromTo 1 10000)

