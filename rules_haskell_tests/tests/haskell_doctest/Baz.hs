{-# LANGUAGE CPP #-}

module Baz (baz) where

-- |
-- >>> baz
-- 101

baz :: Int
#ifndef MAGIC_DOCTEST_THING
baz = 100
#else
baz = 101
#endif
