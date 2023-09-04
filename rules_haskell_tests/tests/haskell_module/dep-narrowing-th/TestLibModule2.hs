module TestLibModule2 where

import SimpleFoo

foo2 :: IO Int
foo2 = (\x -> x - 2) <$> foo
