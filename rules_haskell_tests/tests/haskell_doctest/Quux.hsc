-- | This module is in a .hsc file, this way we test that improt directories
-- are passed correctly to the doctest executable.

module Quux (quux) where

-- |
-- >>> quux
-- 68

quux :: Int
quux = 68
