{-# LANGUAGE TemplateHaskell #-}
-- | "LibB" doc.

module LibB where

import LibA.A (a)
import LibA (f)
import TH (foo)

-- | Doc for 'x' using 'f' and 'a' and 'Int'.
x :: Int
x = const f a

-- | A thing generated with TH.
z :: String
z = $foo
