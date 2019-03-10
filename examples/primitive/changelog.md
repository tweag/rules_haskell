## Changes in version 0.6.4.0

 * Introduce `Data.Primitive.PrimArray`, which offers types and function
   for dealing with a `ByteArray` tagged with a phantom type variable for
   tracking the element type.

 * Implement `isByteArrayPinned` and `isMutableByteArrayPinned`.

 * Add `Eq1`, `Ord1`, `Show1`, and `Read1` instances for `Array` and
   `SmallArray`.

 * Improve the test suite. This includes having property tests for
   typeclasses from `base` such as `Eq`, `Ord`, `Functor`, `Applicative`,
   `Monad`, `IsList`, `Monoid`, `Foldable`, and `Traversable`.

 * Fix the broken `IsList` instance for `ByteArray`. The old definition
   would allocate a byte array of the correct size and then leave the
   memory unitialized instead of writing the list elements to it.

 * Fix the broken `Functor` instance for `Array`. The old definition
   would allocate an array of the correct size with thunks for erroring
   installed at every index. It failed to replace these thunks with
   the result of the function applied to the elements of the argument array.

 * Fix the broken `Applicative` instances of `Array` and `SmallArray`.
   The old implementation of `<*>` for `Array` failed to initialize
   some elements but correctly initialized others in the resulting
   `Array`. It is unclear what the old behavior of `<*>` was for
   `SmallArray`, but it was incorrect.

 * Fix the broken `Monad` instances for `Array` and `SmallArray`.

 * Fix the implementation of `foldl1` in the `Foldable` instances for
   `Array` and `SmallArray`. In both cases, the old implementation
   simply returned the first element of the array and made no use of
   the other elements in the array.

 * Fix the implementation of `mconcat` in the `Monoid` instance for
   `SmallArray`.
 
 * Implement `Data.Primitive.Ptr`, implementations of `Ptr` functions
   that require a `Prim` constraint instead of a `Storable` constraint.


 * Add `PrimUnlifted` instances for `TVar` and `MVar`.

 * Use `compareByteArrays#` for the `Eq` and `Ord` instances of
   `ByteArray` when building with GHC 8.4 and newer.

 * Add `Prim` instances for lots of types in `Foreign.C.Types` and
   `System.Posix.Types`.

 * Reexport `Data.Primitive.SmallArray` and `Data.Primitive.UnliftedArray`
   from `Data.Primitive`.

 * Add fold functions and map function to `Data.Primitive.UnliftedArray`.
   Add typeclass instances for `IsList`, `Ord`, and `Show`.

 * Add `defaultSetByteArray#` and `defaultSetOffAddr#` to
   `Data.Primitive.Types`.

## Changes in version 0.6.3.0

 * Add `PrimMonad` instances for `ContT`, `AccumT`, and `SelectT` from
   `transformers`

 * Add `Eq`, `Ord`, `Show`, and `IsList` instances for `ByteArray`

 * Add `Semigroup` instances for `Array` and `SmallArray`. This allows
   `primitive` to build on GHC 8.4 and later.

## Changes in version 0.6.2.0

 * Drop support for GHCs before 7.4

 * `SmallArray` support

 * `ArrayArray#` based support for more efficient arrays of unlifted pointer types

 * Make `Array` and the like instances of various classes for convenient use

 * Add `Prim` instances for Ptr and FunPtr

 * Add `ioToPrim`, `stToPrim` and unsafe counterparts for situations that would
   otherwise require type ascriptions on `primToPrim`

 * Add `evalPrim`

 * Add `PrimBase` instance for `IdentityT`

## Changes in version 0.6.1.0

 * Use more appropriate types in internal memset functions, which prevents
   overflows/segfaults on 64-bit systems.

 * Fixed a warning on GHC 7.10

 * Worked around a -dcore-lint bug in GHC 7.6/7.7

## Changes in version 0.6

 * Split PrimMonad into two classes to allow automatic lifting of primitive
   operations into monad transformers. The `internal` operation has moved to the
   `PrimBase` class.

 * Fixed the test suite on older GHCs

## Changes in version 0.5.4.0

 * Changed primitive_ to work around an oddity with GHC's code generation
   on certain versions that led to side effects not happening when used
   in conjunction with certain very unsafe IO performers.

 * Allow primitive to build on GHC 7.9

## Changes in version 0.5.3.0

 * Implement `cloneArray` and `cloneMutableArray` primitives
   (with fall-back implementations for GHCs prior to version 7.2.1)

## Changes in version 0.5.2.1

 * Add strict variants of `MutVar` modification functions
   `atomicModifyMutVar'` and `modifyMutVar'`

 * Fix compilation on Solaris 10 with GNU C 3.4.3

## Changes in version 0.5.1.0

 * Add support for GHC 7.7's new primitive `Bool` representation

## Changes in version 0.5.0.1

 * Disable array copying primitives for GHC 7.6.* and earlier

## Changes in version 0.5

 * New in `Data.Primitive.MutVar`: `atomicModifyMutVar`

 * Efficient block fill operations: `setByteArray`, `setAddr`

## Changes in version 0.4.1

 * New module `Data.Primitive.MutVar`

## Changes in version 0.4.0.1

 * Critical bug fix in `fillByteArray`

## Changes in version 0.4

 * Support for GHC 7.2 array copying primitives

 * New in `Data.Primitive.ByteArray`: `copyByteArray`,
   `copyMutableByteArray`, `moveByteArray`, `fillByteArray`

 * Deprecated in `Data.Primitive.ByteArray`: `memcpyByteArray`,
   `memcpyByteArray'`, `memmoveByteArray`, `memsetByteArray`

 * New in `Data.Primitive.Array`: `copyArray`, `copyMutableByteArray`

 * New in `Data.Primitive.Addr`: `copyAddr`, `moveAddr`

 * Deprecated in `Data.Primitive.Addr`: `memcpyAddr`
