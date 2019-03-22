-- base is not available when no dependencies, so we have to define everything
-- from scratch.
{-# LANGUAGE NoImplicitPrelude #-}

module Bool where

data Bool = False | True

not :: Bool -> Bool
not False = True
not True = False

and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False

or :: Bool -> Bool -> Bool
or False False = False
or _ _ = True
