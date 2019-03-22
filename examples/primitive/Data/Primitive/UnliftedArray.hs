{-# Language BangPatterns #-}
{-# Language CPP #-}
{-# Language DeriveDataTypeable #-}
{-# Language MagicHash #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
{-# Language UnboxedTuples #-}

-- |
-- Module      : Data.Primitive.UnliftedArray
-- Copyright   : (c) Dan Doel 2016
-- License     : BSD-style
--
-- Maintainer  : Libraries <libraries@haskell.org>
-- Portability : non-portable
--
-- GHC contains three general classes of value types:
--
--   1. Unboxed types: values are machine values made up of fixed numbers of bytes
--   2. Unlifted types: values are pointers, but strictly evaluated
--   3. Lifted types: values are pointers, lazily evaluated
--
-- The first category can be stored in a 'ByteArray', and this allows types in
-- category 3 that are simple wrappers around category 1 types to be stored
-- more efficiently using a 'ByteArray'. This module provides the same facility
-- for category 2 types.
--
-- GHC has two primitive types, 'ArrayArray#' and 'MutableArrayArray#'. These
-- are arrays of pointers, but of category 2 values, so they are known to not
-- be bottom. This allows types that are wrappers around such types to be stored
-- in an array without an extra level of indirection.
--
-- The way that the 'ArrayArray#' API works is that one can read and write
-- 'ArrayArray#' values to the positions. This works because all category 2
-- types share a uniform representation, unlike unboxed values which are
-- represented by varying (by type) numbers of bytes. However, using the
-- this makes the internal API very unsafe to use, as one has to coerce values
-- to and from 'ArrayArray#'.
--
-- The API presented by this module is more type safe. 'UnliftedArray' and
-- 'MutableUnliftedArray' are parameterized by the type of arrays they contain, and
-- the coercions necessary are abstracted into a class, 'PrimUnlifted', of things
-- that are eligible to be stored.

module Data.Primitive.UnliftedArray
  ( -- * Types
    UnliftedArray(..)
  , MutableUnliftedArray(..)
  , PrimUnlifted(..)
    -- * Operations
  , unsafeNewUnliftedArray
  , newUnliftedArray
  , setUnliftedArray
  , sizeofUnliftedArray
  , sizeofMutableUnliftedArray
  , readUnliftedArray
  , writeUnliftedArray
  , indexUnliftedArray
  , indexUnliftedArrayM
  , unsafeFreezeUnliftedArray
  , freezeUnliftedArray
  , thawUnliftedArray
  , runUnliftedArray
  , sameMutableUnliftedArray
  , copyUnliftedArray
  , copyMutableUnliftedArray
  , cloneUnliftedArray
  , cloneMutableUnliftedArray
    -- * List Conversion
  , unliftedArrayToList
  , unliftedArrayFromList
  , unliftedArrayFromListN
    -- * Folding
  , foldrUnliftedArray
  , foldrUnliftedArray'
  , foldlUnliftedArray
  , foldlUnliftedArray'
    -- * Mapping
  , mapUnliftedArray
-- Missing operations:
--  , unsafeThawUnliftedArray
  ) where

import Data.Typeable
import Control.Applicative

import GHC.Prim
import GHC.Base (Int(..),build)

import Control.Monad.Primitive

import Control.Monad.ST (runST,ST)

import Data.Monoid (Monoid,mappend)
import Data.Primitive.Internal.Compat ( isTrue# )

import qualified Data.List as L
import           Data.Primitive.Array (Array)
import qualified Data.Primitive.Array as A
import           Data.Primitive.ByteArray (ByteArray)
import qualified Data.Primitive.ByteArray as BA
import qualified Data.Primitive.PrimArray as PA
import qualified Data.Primitive.SmallArray as SA
import qualified Data.Primitive.MutVar as MV
import qualified Data.Monoid
import qualified GHC.MVar as GM (MVar(..))
import qualified GHC.Conc as GC (TVar(..))
import qualified GHC.Stable as GSP (StablePtr(..))
import qualified GHC.Weak as GW (Weak(..))
import qualified GHC.Conc.Sync as GCS (ThreadId(..))
import qualified GHC.Exts as E
import qualified GHC.ST as GHCST

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup
#endif

#if MIN_VERSION_base(4,10,0)
import GHC.Exts (runRW#)
#elif MIN_VERSION_base(4,9,0)
import GHC.Base (runRW#)
#endif

-- | Immutable arrays that efficiently store types that are simple wrappers
-- around unlifted primitive types. The values of the unlifted type are
-- stored directly, eliminating a layer of indirection.
data UnliftedArray e = UnliftedArray ArrayArray#
  deriving (Typeable)

-- | Mutable arrays that efficiently store types that are simple wrappers
-- around unlifted primitive types. The values of the unlifted type are
-- stored directly, eliminating a layer of indirection.
data MutableUnliftedArray s e = MutableUnliftedArray (MutableArrayArray# s)
  deriving (Typeable)

-- | Classifies the types that are able to be stored in 'UnliftedArray' and
-- 'MutableUnliftedArray'. These should be types that are just liftings of the
-- unlifted pointer types, so that their internal contents can be safely coerced
-- into an 'ArrayArray#'.
class PrimUnlifted a where
  toArrayArray# :: a -> ArrayArray#
  fromArrayArray# :: ArrayArray# -> a

instance PrimUnlifted (UnliftedArray e) where
  toArrayArray# (UnliftedArray aa#) = aa#
  fromArrayArray# aa# = UnliftedArray aa#

instance PrimUnlifted (MutableUnliftedArray s e) where
  toArrayArray# (MutableUnliftedArray maa#) = unsafeCoerce# maa#
  fromArrayArray# aa# = MutableUnliftedArray (unsafeCoerce# aa#)

instance PrimUnlifted (Array a) where
  toArrayArray# (A.Array a#) = unsafeCoerce# a#
  fromArrayArray# aa# = A.Array (unsafeCoerce# aa#)

instance PrimUnlifted (A.MutableArray s a) where
  toArrayArray# (A.MutableArray ma#) = unsafeCoerce# ma#
  fromArrayArray# aa# = A.MutableArray (unsafeCoerce# aa#)

instance PrimUnlifted ByteArray where
  toArrayArray# (BA.ByteArray ba#) = unsafeCoerce# ba#
  fromArrayArray# aa# = BA.ByteArray (unsafeCoerce# aa#)

instance PrimUnlifted (BA.MutableByteArray s) where
  toArrayArray# (BA.MutableByteArray mba#) = unsafeCoerce# mba#
  fromArrayArray# aa# = BA.MutableByteArray (unsafeCoerce# aa#)

-- | @since 0.6.4.0
instance PrimUnlifted (PA.PrimArray a) where
  toArrayArray# (PA.PrimArray ba#) = unsafeCoerce# ba#
  fromArrayArray# aa# = PA.PrimArray (unsafeCoerce# aa#)

-- | @since 0.6.4.0
instance PrimUnlifted (PA.MutablePrimArray s a) where
  toArrayArray# (PA.MutablePrimArray mba#) = unsafeCoerce# mba#
  fromArrayArray# aa# = PA.MutablePrimArray (unsafeCoerce# aa#)

instance PrimUnlifted (SA.SmallArray a) where
  toArrayArray# (SA.SmallArray sa#) = unsafeCoerce# sa#
  fromArrayArray# aa# = SA.SmallArray (unsafeCoerce# aa#)

instance PrimUnlifted (SA.SmallMutableArray s a) where
  toArrayArray# (SA.SmallMutableArray sma#) = unsafeCoerce# sma#
  fromArrayArray# aa# = SA.SmallMutableArray (unsafeCoerce# aa#)

instance PrimUnlifted (MV.MutVar s a) where
  toArrayArray# (MV.MutVar mv#) = unsafeCoerce# mv#
  fromArrayArray# aa# = MV.MutVar (unsafeCoerce# aa#)

-- | @since 0.6.4.0
instance PrimUnlifted (GM.MVar a) where
  toArrayArray# (GM.MVar mv#) = unsafeCoerce# mv#
  fromArrayArray# mv# = GM.MVar (unsafeCoerce# mv#)

-- | @since 0.6.4.0
instance PrimUnlifted (GC.TVar a) where
  toArrayArray# (GC.TVar tv#) = unsafeCoerce# tv#
  fromArrayArray# tv# = GC.TVar (unsafeCoerce# tv#)

-- | @since 0.6.4.0
instance PrimUnlifted (GSP.StablePtr a) where
  toArrayArray# (GSP.StablePtr tv#) = unsafeCoerce# tv#
  fromArrayArray# tv# = GSP.StablePtr (unsafeCoerce# tv#)

-- | @since 0.6.4.0
instance PrimUnlifted (GW.Weak a) where
  toArrayArray# (GW.Weak tv#) = unsafeCoerce# tv#
  fromArrayArray# tv# = GW.Weak (unsafeCoerce# tv#)

-- | @since 0.6.4.0
instance PrimUnlifted GCS.ThreadId where
  toArrayArray# (GCS.ThreadId tv#) = unsafeCoerce# tv#
  fromArrayArray# tv# = GCS.ThreadId (unsafeCoerce# tv#)

die :: String -> String -> a
die fun problem = error $ "Data.Primitive.UnliftedArray." ++ fun ++ ": " ++ problem

-- | Creates a new 'MutableUnliftedArray'. This function is unsafe because it
-- initializes all elements of the array as pointers to the array itself. Attempting
-- to read one of these elements before writing to it is in effect an unsafe
-- coercion from the @MutableUnliftedArray s a@ to the element type.
unsafeNewUnliftedArray
  :: (PrimMonad m)
  => Int -- ^ size
  -> m (MutableUnliftedArray (PrimState m) a)
unsafeNewUnliftedArray (I# i#) = primitive $ \s -> case newArrayArray# i# s of
  (# s', maa# #) -> (# s', MutableUnliftedArray maa# #)
{-# inline unsafeNewUnliftedArray #-}

-- | Sets all the positions in an unlifted array to the designated value.
setUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> a -- ^ value to fill with
  -> m ()
setUnliftedArray mua v = loop $ sizeofMutableUnliftedArray mua - 1
 where
 loop i | i < 0     = return ()
        | otherwise = writeUnliftedArray mua i v >> loop (i-1)
{-# inline setUnliftedArray #-}

-- | Creates a new 'MutableUnliftedArray' with the specified value as initial
-- contents. This is slower than 'unsafeNewUnliftedArray', but safer.
newUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => Int -- ^ size
  -> a -- ^ initial value
  -> m (MutableUnliftedArray (PrimState m) a)
newUnliftedArray len v =
  unsafeNewUnliftedArray len >>= \mua -> setUnliftedArray mua v >> return mua
{-# inline newUnliftedArray #-}

-- | Yields the length of an 'UnliftedArray'.
sizeofUnliftedArray :: UnliftedArray e -> Int
sizeofUnliftedArray (UnliftedArray aa#) = I# (sizeofArrayArray# aa#)
{-# inline sizeofUnliftedArray #-}

-- | Yields the length of a 'MutableUnliftedArray'.
sizeofMutableUnliftedArray :: MutableUnliftedArray s e -> Int
sizeofMutableUnliftedArray (MutableUnliftedArray maa#)
  = I# (sizeofMutableArrayArray# maa#)
{-# inline sizeofMutableUnliftedArray #-}

-- Internal indexing function.
--
-- Note: ArrayArray# is strictly evaluated, so this should have similar
-- consequences to indexArray#, where matching on the unboxed single causes the
-- array access to happen.
indexUnliftedArrayU
  :: PrimUnlifted a
  => UnliftedArray a
  -> Int
  -> (# a #)
indexUnliftedArrayU (UnliftedArray src#) (I# i#)
  = case indexArrayArrayArray# src# i# of
      aa# -> (# fromArrayArray# aa# #)
{-# inline indexUnliftedArrayU #-}

-- | Gets the value at the specified position of an 'UnliftedArray'.
indexUnliftedArray
  :: PrimUnlifted a
  => UnliftedArray a -- ^ source
  -> Int -- ^ index
  -> a
indexUnliftedArray ua i
  = case indexUnliftedArrayU ua i of (# v #) -> v
{-# inline indexUnliftedArray #-}

-- | Gets the value at the specified position of an 'UnliftedArray'.
-- The purpose of the 'Monad' is to allow for being eager in the
-- 'UnliftedArray' value without having to introduce a data dependency
-- directly on the result value.
--
-- It should be noted that this is not as much of a problem as with a normal
-- 'Array', because elements of an 'UnliftedArray' are guaranteed to not
-- be exceptional. This function is provided in case it is more desirable
-- than being strict in the result value.
indexUnliftedArrayM
  :: (PrimUnlifted a, Monad m)
  => UnliftedArray a -- ^ source
  -> Int -- ^ index
  -> m a
indexUnliftedArrayM ua i
  = case indexUnliftedArrayU ua i of
      (# v #) -> return v
{-# inline indexUnliftedArrayM #-}

-- | Gets the value at the specified position of a 'MutableUnliftedArray'.
readUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ index
  -> m a
readUnliftedArray (MutableUnliftedArray maa#) (I# i#)
  = primitive $ \s -> case readArrayArrayArray# maa# i# s of
      (# s', aa# #) -> (# s',  fromArrayArray# aa# #)
{-# inline readUnliftedArray #-}

-- | Sets the value at the specified position of a 'MutableUnliftedArray'.
writeUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> Int -- ^ index
  -> a -- ^ value
  -> m ()
writeUnliftedArray (MutableUnliftedArray maa#) (I# i#) a
  = primitive_ (writeArrayArrayArray# maa# i# (toArrayArray# a))
{-# inline writeUnliftedArray #-}

-- | Freezes a 'MutableUnliftedArray', yielding an 'UnliftedArray'. This simply
-- marks the array as frozen in place, so it should only be used when no further
-- modifications to the mutable array will be performed.
unsafeFreezeUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a
  -> m (UnliftedArray a)
unsafeFreezeUnliftedArray (MutableUnliftedArray maa#)
  = primitive $ \s -> case unsafeFreezeArrayArray# maa# s of
      (# s', aa# #) -> (# s', UnliftedArray aa# #)
{-# inline unsafeFreezeUnliftedArray #-}

-- | Determines whether two 'MutableUnliftedArray' values are the same. This is
-- object/pointer identity, not based on the contents.
sameMutableUnliftedArray
  :: MutableUnliftedArray s a
  -> MutableUnliftedArray s a
  -> Bool
sameMutableUnliftedArray (MutableUnliftedArray maa1#) (MutableUnliftedArray maa2#)
  = isTrue# (sameMutableArrayArray# maa1# maa2#)
{-# inline sameMutableUnliftedArray #-}

-- | Copies the contents of an immutable array into a mutable array.
copyUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> Int -- ^ offset into destination
  -> UnliftedArray a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> m ()
copyUnliftedArray
  (MutableUnliftedArray dst) (I# doff)
  (UnliftedArray src) (I# soff) (I# ln) =
    primitive_ $ copyArrayArray# src soff dst doff ln
{-# inline copyUnliftedArray #-}

-- | Copies the contents of one mutable array into another.
copyMutableUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> Int -- ^ offset into destination
  -> MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> m ()
copyMutableUnliftedArray
  (MutableUnliftedArray dst) (I# doff)
  (MutableUnliftedArray src) (I# soff) (I# ln) =
    primitive_ $ copyMutableArrayArray# src soff dst doff ln
{-# inline copyMutableUnliftedArray #-}

-- | Freezes a portion of a 'MutableUnliftedArray', yielding an 'UnliftedArray'.
-- This operation is safe, in that it copies the frozen portion, and the
-- existing mutable array may still be used afterward.
freezeUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (UnliftedArray a)
freezeUnliftedArray src off len = do
  dst <- unsafeNewUnliftedArray len
  copyMutableUnliftedArray dst 0 src off len
  unsafeFreezeUnliftedArray dst
{-# inline freezeUnliftedArray #-}

-- | Thaws a portion of an 'UnliftedArray', yielding a 'MutableUnliftedArray'.
-- This copies the thawed portion, so mutations will not affect the original
-- array.
thawUnliftedArray
  :: (PrimMonad m)
  => UnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (MutableUnliftedArray (PrimState m) a)
thawUnliftedArray src off len = do
  dst <- unsafeNewUnliftedArray len
  copyUnliftedArray dst 0 src off len
  return dst
{-# inline thawUnliftedArray #-}

#if !MIN_VERSION_base(4,9,0)
unsafeCreateUnliftedArray
  :: Int
  -> (forall s. MutableUnliftedArray s a -> ST s ())
  -> UnliftedArray a
unsafeCreateUnliftedArray 0 _ = emptyUnliftedArray
unsafeCreateUnliftedArray n f = runUnliftedArray $ do
  mary <- unsafeNewUnliftedArray n
  f mary
  pure mary

-- | Execute a stateful computation and freeze the resulting array.
runUnliftedArray
  :: (forall s. ST s (MutableUnliftedArray s a))
  -> UnliftedArray a
runUnliftedArray m = runST $ m >>= unsafeFreezeUnliftedArray

#else /* Below, runRW# is available. */

-- This low-level business is designed to work with GHC's worker-wrapper
-- transformation. A lot of the time, we don't actually need an Array
-- constructor. By putting it on the outside, and being careful about
-- how we special-case the empty array, we can make GHC smarter about this.
-- The only downside is that separately created 0-length arrays won't share
-- their Array constructors, although they'll share their underlying
-- Array#s.
unsafeCreateUnliftedArray
  :: Int
  -> (forall s. MutableUnliftedArray s a -> ST s ())
  -> UnliftedArray a
unsafeCreateUnliftedArray 0 _ = UnliftedArray (emptyArrayArray# (# #))
unsafeCreateUnliftedArray n f = runUnliftedArray $ do
  mary <- unsafeNewUnliftedArray n
  f mary
  pure mary

-- | Execute a stateful computation and freeze the resulting array.
runUnliftedArray
  :: (forall s. ST s (MutableUnliftedArray s a))
  -> UnliftedArray a
runUnliftedArray m = UnliftedArray (runUnliftedArray# m)

runUnliftedArray#
  :: (forall s. ST s (MutableUnliftedArray s a))
  -> ArrayArray#
runUnliftedArray# m = case runRW# $ \s ->
  case unST m s of { (# s', MutableUnliftedArray mary# #) ->
  unsafeFreezeArrayArray# mary# s'} of (# _, ary# #) -> ary#

unST :: ST s a -> State# s -> (# State# s, a #)
unST (GHCST.ST f) = f

emptyArrayArray# :: (# #) -> ArrayArray#
emptyArrayArray# _ = case emptyUnliftedArray of UnliftedArray ar -> ar
{-# NOINLINE emptyArrayArray# #-}
#endif

-- | Creates a copy of a portion of an 'UnliftedArray'
cloneUnliftedArray
  :: UnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> UnliftedArray a
cloneUnliftedArray src off len =
  runUnliftedArray (thawUnliftedArray src off len)
{-# inline cloneUnliftedArray #-}

-- | Creates a new 'MutableUnliftedArray' containing a copy of a portion of
-- another mutable array.
cloneMutableUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (MutableUnliftedArray (PrimState m) a)
cloneMutableUnliftedArray src off len = do
  dst <- unsafeNewUnliftedArray len
  copyMutableUnliftedArray dst 0 src off len
  return dst
{-# inline cloneMutableUnliftedArray #-}

instance Eq (MutableUnliftedArray s a) where
  (==) = sameMutableUnliftedArray

instance (Eq a, PrimUnlifted a) => Eq (UnliftedArray a) where
  aa1 == aa2 = sizeofUnliftedArray aa1 == sizeofUnliftedArray aa2
            && loop (sizeofUnliftedArray aa1 - 1)
   where
   loop i
     | i < 0 = True
     | otherwise = indexUnliftedArray aa1 i == indexUnliftedArray aa2 i && loop (i-1)

-- | Lexicographic ordering. Subject to change between major versions.
--
--   @since 0.6.4.0
instance (Ord a, PrimUnlifted a) => Ord (UnliftedArray a) where
  compare a1 a2 = loop 0
    where
    mn = sizeofUnliftedArray a1 `min` sizeofUnliftedArray a2
    loop i
      | i < mn
      , x1 <- indexUnliftedArray a1 i
      , x2 <- indexUnliftedArray a2 i
      = compare x1 x2 `mappend` loop (i+1)
      | otherwise = compare (sizeofUnliftedArray a1) (sizeofUnliftedArray a2)

-- | @since 0.6.4.0
instance (Show a, PrimUnlifted a) => Show (UnliftedArray a) where
  showsPrec p a = showParen (p > 10) $
    showString "fromListN " . shows (sizeofUnliftedArray a) . showString " "
      . shows (unliftedArrayToList a)

#if MIN_VERSION_base(4,9,0)
-- | @since 0.6.4.0
instance PrimUnlifted a => Semigroup (UnliftedArray a) where
  (<>) = concatUnliftedArray
#endif

-- | @since 0.6.4.0
instance PrimUnlifted a => Monoid (UnliftedArray a) where
  mempty = emptyUnliftedArray
#if !(MIN_VERSION_base(4,11,0))
  mappend = concatUnliftedArray
#endif

emptyUnliftedArray :: UnliftedArray a
emptyUnliftedArray = runUnliftedArray (unsafeNewUnliftedArray 0)
{-# NOINLINE emptyUnliftedArray #-}

concatUnliftedArray :: UnliftedArray a -> UnliftedArray a -> UnliftedArray a
concatUnliftedArray x y = unsafeCreateUnliftedArray (sizeofUnliftedArray x + sizeofUnliftedArray y) $ \m -> do
  copyUnliftedArray m 0 x 0 (sizeofUnliftedArray x)
  copyUnliftedArray m (sizeofUnliftedArray x) y 0 (sizeofUnliftedArray y)

-- | Lazy right-associated fold over the elements of an 'UnliftedArray'.
{-# INLINE foldrUnliftedArray #-}
foldrUnliftedArray :: forall a b. PrimUnlifted a => (a -> b -> b) -> b -> UnliftedArray a -> b
foldrUnliftedArray f z arr = go 0
  where
    !sz = sizeofUnliftedArray arr
    go !i
      | sz > i = f (indexUnliftedArray arr i) (go (i+1))
      | otherwise = z

-- | Strict right-associated fold over the elements of an 'UnliftedArray.
{-# INLINE foldrUnliftedArray' #-}
foldrUnliftedArray' :: forall a b. PrimUnlifted a => (a -> b -> b) -> b -> UnliftedArray a -> b
foldrUnliftedArray' f z0 arr = go (sizeofUnliftedArray arr - 1) z0
  where
    go !i !acc
      | i < 0 = acc
      | otherwise = go (i - 1) (f (indexUnliftedArray arr i) acc)

-- | Lazy left-associated fold over the elements of an 'UnliftedArray'.
{-# INLINE foldlUnliftedArray #-}
foldlUnliftedArray :: forall a b. PrimUnlifted a => (b -> a -> b) -> b -> UnliftedArray a -> b
foldlUnliftedArray f z arr = go (sizeofUnliftedArray arr - 1)
  where
    go !i
      | i < 0 = z
      | otherwise = f (go (i - 1)) (indexUnliftedArray arr i)

-- | Strict left-associated fold over the elements of an 'UnliftedArray'.
{-# INLINE foldlUnliftedArray' #-}
foldlUnliftedArray' :: forall a b. PrimUnlifted a => (b -> a -> b) -> b -> UnliftedArray a -> b
foldlUnliftedArray' f z0 arr = go 0 z0
  where
    !sz = sizeofUnliftedArray arr
    go !i !acc
      | i < sz = go (i + 1) (f acc (indexUnliftedArray arr i))
      | otherwise = acc

-- | Map over the elements of an 'UnliftedArray'.
{-# INLINE mapUnliftedArray #-}
mapUnliftedArray :: (PrimUnlifted a, PrimUnlifted b)
  => (a -> b)
  -> UnliftedArray a
  -> UnliftedArray b
mapUnliftedArray f arr = unsafeCreateUnliftedArray sz $ \marr -> do
  let go !ix = if ix < sz
        then do
          let b = f (indexUnliftedArray arr ix)
          writeUnliftedArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  where
  !sz = sizeofUnliftedArray arr

-- | Convert the unlifted array to a list.
{-# INLINE unliftedArrayToList #-}
unliftedArrayToList :: PrimUnlifted a => UnliftedArray a -> [a]
unliftedArrayToList xs = build (\c n -> foldrUnliftedArray c n xs)

unliftedArrayFromList :: PrimUnlifted a => [a] -> UnliftedArray a
unliftedArrayFromList xs = unliftedArrayFromListN (L.length xs) xs

unliftedArrayFromListN :: forall a. PrimUnlifted a => Int -> [a] -> UnliftedArray a
unliftedArrayFromListN len vs = unsafeCreateUnliftedArray len run where
  run :: forall s. MutableUnliftedArray s a -> ST s ()
  run arr = do
    let go :: [a] -> Int -> ST s ()
        go [] !ix = if ix == len
          -- The size check is mandatory since failure to initialize all elements
          -- introduces the possibility of a segfault happening when someone attempts
          -- to read the unitialized element. See the docs for unsafeNewUnliftedArray.
          then return ()
          else die "unliftedArrayFromListN" "list length less than specified size"
        go (a : as) !ix = if ix < len
          then do
            writeUnliftedArray arr ix a
            go as (ix + 1)
          else die "unliftedArrayFromListN" "list length greater than specified size"
    go vs 0


#if MIN_VERSION_base(4,7,0)
-- | @since 0.6.4.0
instance PrimUnlifted a => E.IsList (UnliftedArray a) where
  type Item (UnliftedArray a) = a
  fromList = unliftedArrayFromList
  fromListN = unliftedArrayFromListN
  toList = unliftedArrayToList
#endif

