{-# LANGUAGE TemplateHaskell #-}
-- | "LibB" doc.

module LibB where

import LibA.A (a)
import LibA (f)
import LibC (mytype, LibCType)
import TH (foo)

-- | Doc for 'x' using 'f' and 'a' and 'Int'.
x :: Int
x = const f a

-- | This uses a type from an undocumented package
y :: LibCType
y = mytype

-- | A thing generated with TH.
z :: String
z = $foo
