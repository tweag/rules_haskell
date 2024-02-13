{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PrimArray.Compare
  ( benchmarkLt
  , benchmarkLtDef
  , benchmarkLte
  , benchmarkLteDef
  , argumentA
  , argumentB
  ) where

import Data.Primitive
import Data.Word
import Control.Monad
import Control.Monad.ST (runST)
import GHC.Exts (fromList)

benchmarkLtDef :: PrimArray Int -> PrimArray Int -> Bool
benchmarkLtDef a b = case compare a b of
  LT -> True
  _ -> False

benchmarkLteDef :: PrimArray Int -> PrimArray Int -> Bool
benchmarkLteDef a b = case compare a b of
  GT -> False
  _ -> True

benchmarkLt :: PrimArray Int -> PrimArray Int -> Bool
benchmarkLt a b =
  let !sz1 = sizeofPrimArray a
      !sz2 = sizeofPrimArray b
      !sz = min sz1 sz2
      loop !i
        | i < sz = if indexPrimArray a i < indexPrimArray b i
            then True
            else loop (i + 1)
        | otherwise = sz1 < sz2
   in loop 0

benchmarkLte :: PrimArray Int -> PrimArray Int -> Bool
benchmarkLte a b =
  let !sz1 = sizeofPrimArray a
      !sz2 = sizeofPrimArray b
      !sz = min sz1 sz2
      loop !i
        | i < sz = if indexPrimArray a i <= indexPrimArray b i
            then loop (i + 1)
            else False
        | otherwise = sz1 < sz2
   in loop 0

argumentA :: PrimArray Int
argumentA = fromList (enumFromTo 0 8000 ++ [55])

argumentB :: PrimArray Int
argumentB = fromList (enumFromTo 0 8000 ++ [56])

