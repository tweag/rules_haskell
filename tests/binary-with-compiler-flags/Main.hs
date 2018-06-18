-- Expects to pull -XStandaloneDeriving from the default compiler flags.
module Main (main) where

data Foo = Foo
deriving instance Show Foo

main :: IO ()
main = print Foo
