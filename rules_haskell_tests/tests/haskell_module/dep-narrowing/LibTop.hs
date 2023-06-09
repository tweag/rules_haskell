module LibTop(bar, bar2, foo2) where

import TestModule

-- Requiring foo2 tests that the interface file for the cross-library
-- dep TestLibModule2.foo2 is exposed when building this module.
--
-- Similarly for depFoo, we test that the interface file of the transitive
-- dependency DepFoo is available.
bar2 :: Int
bar2 = foo2 + depFoo
