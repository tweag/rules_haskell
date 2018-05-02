-- | "Lib" header
{-# LANGUAGE TemplateHaskell #-}

module LibA where

import LibA.A (a)
import Deep (deep_lib)

-- | 'A' declaration.
data A =
  -- | 'A' constructor.
  A

-- | Doc for 'f' using 'a' and 'deep_lib'.
f :: Int
f = const $a deep_lib
