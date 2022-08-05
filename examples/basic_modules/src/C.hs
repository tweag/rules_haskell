module C where

import qualified B

data N = Z | S N

f :: a -> a -> a
f x = B.g x x
