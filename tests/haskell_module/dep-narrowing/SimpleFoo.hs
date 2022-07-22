module SimpleFoo(foo, depFoo) where

import DepFoo

foo :: Int
foo = depFoo
