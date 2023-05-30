-- Expects to pull -XStandaloneDeriving from the default compiler flags.
module Main (main) where

data Foo = Foo
deriving instance Show Foo

-- Expects -XLambdaCase to be passed via the 'compiler_flags' rule attribute.
dummyId :: Foo -> Foo
dummyId = \case
  Foo -> Foo

main :: IO ()
main = print $ dummyId Foo
