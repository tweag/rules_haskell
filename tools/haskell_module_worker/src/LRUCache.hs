-- | An LRU cache
module LRUCache
  ( LRUCache(..)
  , get
  , discard
  ) where

import Data.List (partition)
import Data.Word (Word64)

data LRUCache k v = LRUCache
  { lruCapacity :: Word64          -- ^ Amount of elements admitted by the cache
  , lruElements :: [(k, v)]        -- ^ Elements in the cache
  , lruAlloc :: k -> IO v          -- ^ Action to allocate new elements
  , lruCleanup :: v -> IO ()       -- ^ Action to cleanup discarded elements
  }

-- | Looks for an element with key @k@ and satisfying the given predicate.
--
-- If the element is not found, the alloc action is called.
-- If the capacity of the cache is exceeded, the least used
-- elements are discarded.
get :: Eq k => k -> (v -> IO Bool) -> LRUCache k v -> IO (LRUCache k v, v)
get k p lru = do
  br <- breakM (\(k', v) -> if k == k' then p v else return False) (lruElements lru)
  case br of
    (prefix, kv@(_, v) : suffix) ->
      return (lru {lruElements = kv : prefix ++ suffix}, v)
    _ -> do
      v <- lruAlloc lru k
      let (toKeep, toDiscard) =
            splitAt (fromIntegral $ lruCapacity lru) ((k, v) : lruElements lru)
      mapM_ (lruCleanup lru . snd) toDiscard
      return (lru {lruElements = toKeep}, v)

-- | Discards the element satisfying a given predicate.
discard :: (v -> Bool) -> LRUCache k v -> IO (LRUCache k v)
discard p lru = do
    let (toDiscard, toKeep) = partition (p . snd) (lruElements lru)
    mapM_ (lruCleanup lru . snd) toDiscard
    return lru {lruElements = toKeep}

-- | @breakM p xs == (ys, zs)@ iff @xs == ys ++ zs@, and @ys@ is the largest
-- prefix containing no elements satisfying @p@.
breakM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
breakM p = go []
  where
    go acc [] = return (reverse acc, [])
    go acc (x:xs) = do
      b <- p x
      if b then
        return (reverse acc, x : xs)
      else
        go (x : acc) xs
