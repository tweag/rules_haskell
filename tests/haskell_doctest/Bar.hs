module Bar (bar) where

-- Regression test for https://github.com/tweag/rules_haskell/issues/936
import Data.Default.Class (def)  -- Stackage dependency
import Data.Functor.Constant  -- GHC package-db dependency

import Foo (foo)
import Numeric

-- |
-- >>> bar
-- 9
-- >>> showInt bar "" ++ "!"
-- "9!"

bar :: Int
bar = getConstant $ Constant $ 4 + foo + def
