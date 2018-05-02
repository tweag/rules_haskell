-- | "LibB" doc
module LibB where

import LibA.A (a)
import LibA (f)

-- | Doc for 'x' using 'f' and 'a' and 'Int'.
x :: Int
x = const f a
