module Bar (bar) where

import Foo (foo)
import Numeric

-- |
-- >>> bar
-- 9
-- >>> showInt bar "" ++ "!"
-- "9!"

bar :: Int
bar = 4 + foo
