module B where

import qualified A (f, g, T)

f :: A.T -> A.T
f x = A.f (A.g x) x

g :: a -> a -> a -> a
g x y = A.f (A.f x) (A.f x)
