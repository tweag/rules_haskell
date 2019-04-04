{-# LANGUAGE TemplateHaskell #-}

module AddOne2
  ( addThree2
  , six
  ) where

import AddThree

addThree2 = addThree

six = [|addThree 3|]
