-- | "Lib" header
module LibA where

import LibA.A (a)

-- | 'A' declaration.
data A =
  -- | 'A' constructor.
  A

-- | Doc for 'f' using 'a'.
f :: ()
f = a
