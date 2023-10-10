module A (f, g, T, a) where

data T = T0 | T1

a :: T
a = T1

f :: a -> b -> a
f x y = x

g :: T -> T
g T0 = a
g T1 = T1
