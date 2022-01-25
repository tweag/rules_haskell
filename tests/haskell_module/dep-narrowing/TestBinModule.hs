import LibTop

-- Requiring both bar and foo2 tests that the interface file
-- for the cross-library dep TestLibModule2.foo2 is exposed
-- when building this module.
main :: IO ()
main = print (bar + foo2)
