{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ByteArray.Compare
  ( benchmark
  , argumentSmall
  , argumentMedium
  , argumentLarge
  ) where

import Data.Primitive
import Data.Word
import Control.Monad
import Control.Monad.ST (runST)
import GHC.Exts (fromList)

-- This takes the cross product of the argument with itself
-- and compares each pair of combined ByteArrays. In other words,
-- it compare every ByteArray to every other ByteArray (including
-- itself). This is does efficiently and should not allocate
-- any memory.
benchmark :: Array ByteArray -> Int
benchmark !uarr = outer 0
  where
  sz = sizeofArray uarr
  outer :: Int -> Int
  outer !v0 =
    let go !v !ix = if ix < sz
          then go (inner v (indexArray uarr ix)) (ix + 1)
          else v
     in go v0 0
  inner :: Int -> ByteArray -> Int
  inner !v0 !barr =
    let go !v !ix = if ix < sz
          then
            let !y = case compare barr (indexArray uarr ix) of
                  LT -> (-1)
                  EQ -> 0
                  GT -> 1
             in go (v + y) (ix + 1)
          else v
     in go v0 0

-- This is an array of all byte arrays consistent of the bytes 0 and 1
-- bewteen length 0 and 7 inclusive:
--
-- []
-- [0]
-- [1]
-- [0,0]
-- [0,1]
-- ...
-- [1,1,1,1,1,1,0]
-- [1,1,1,1,1,1,1]
--
-- These are very small byte arrays. All of them are smaller than a
-- cache line. A comparison function that uses the FFI may perform
-- worse on such inputs than one that does not.
argumentSmall :: Array ByteArray
argumentSmall = runST $ do
  let (ys :: [[Word8]]) = foldMap (\n -> replicateM n [0,1]) (enumFromTo 0 7)
  marr <- newArray (length ys) undefined
  let go !_ [] = return ()
      go !ix (x : xs) = do
        writeArray marr ix (fromList x)
        go (ix + 1) xs
  go 0 ys
  unsafeFreezeArray marr


-- This is an array of all byte arrays consistent of the bytes 0 and 1
-- bewteen length 0 and 7 inclusive. However, they are all padded on the
-- left by the same 256 bytes. Comparing any two of them will require
-- walking and comparing the first 256 bytes.
argumentMedium :: Array ByteArray
argumentMedium  = runST $ do
  let (ys :: [[Word8]]) = foldMap (\n -> map (enumFromTo 0 255 ++) (replicateM n [0,1])) (enumFromTo 0 7)
  marr <- newArray (length ys) undefined
  let go !_ [] = return ()
      go !ix (x : xs) = do
        writeArray marr ix (fromList x)
        go (ix + 1) xs
  go 0 ys
  unsafeFreezeArray marr

-- Same thing but with left padding of 1024 bytes.
argumentLarge :: Array ByteArray
argumentLarge  = runST $ do
  let (ys :: [[Word8]]) = foldMap (\n -> map (concat (replicate 4 (enumFromTo 0 255)) ++) (replicateM n [0,1])) (enumFromTo 0 7)
  marr <- newArray (length ys) undefined
  let go !_ [] = return ()
      go !ix (x : xs) = do
        writeArray marr ix (fromList x)
        go (ix + 1) xs
  go 0 ys
  unsafeFreezeArray marr
