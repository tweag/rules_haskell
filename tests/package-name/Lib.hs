{-# LANGUAGE CPP #-}
module Lib (foo, libPackageKey) where

foo :: Integer
foo = 42

libPackageKey :: String
libPackageKey = CURRENT_PACKAGE_KEY
