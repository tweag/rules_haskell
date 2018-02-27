module Baz (bar) where

import Foo (foo)
import Bar (bar)

baz :: Int
baz = foo + bar
