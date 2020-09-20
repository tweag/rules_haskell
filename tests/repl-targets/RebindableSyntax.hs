{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module RebindableSyntax where

import Control.Monad (unless)
import System.Exit (exitFailure, exitSuccess)
import qualified Prelude as Prelude
import Prelude (($), (.))

newtype String = String {runString :: Prelude.String -> Prelude.String}

empty :: String
empty = String Prelude.id

(++) :: String -> String -> String
s1 ++ s2 = String $ runString s1 . runString s2

showString :: String -> Prelude.String
showString s = runString s []

newtype MyMonad a = MyMonad {runMyMonad :: String -> (String, a)}

(>>=) :: MyMonad a -> (a -> MyMonad b) -> MyMonad b
m >>= k = MyMonad $ \s -> let (s', a) = runMyMonad m s in runMyMonad (k a) s'

(>>) :: MyMonad a -> MyMonad b -> MyMonad b
m >> n = m >>= \_ -> n

return :: a -> MyMonad a
return a = MyMonad $ \s -> (s, a)

fromString :: Prelude.String -> MyMonad ()
fromString s' = MyMonad $ \s -> (s ++ String (s' Prelude.++), ())

action :: MyMonad ()
action = do
  "foo"
  bar <- return ['b', 'a', 'r']
  fromString bar

check :: Prelude.Bool
check = showString s Prelude.== ['f', 'o', 'o', 'b', 'a', 'r']
  where
    (s, ()) = runMyMonad action empty
