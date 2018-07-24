-- | "LibB" doc
module LibB where

import LibA.A (a)
import LibA (f)
import LibC (mytype, LibCType)

-- | Doc for 'x' using 'f' and 'a' and 'Int'.
x :: Int
x = const f a

-- | This uses a type from an undocumented package
y :: LibCType
y = mytype
