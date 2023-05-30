{-# LANGUAGE TemplateHaskell #-}

module TH (foo) where

import IntLib (crc)
import Language.Haskell.TH

foo :: Q Exp
foo = [| crc |]
