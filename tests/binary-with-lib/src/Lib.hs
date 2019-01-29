{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib (value) where

import Language.Haskell.TH

value = [|42|]
