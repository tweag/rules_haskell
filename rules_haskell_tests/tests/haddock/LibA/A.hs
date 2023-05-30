-- | "LibA.A" header
{-# LANGUAGE TemplateHaskell #-}

module LibA.A where

import Language.Haskell.TH

-- | 'a' doc
a :: Q Exp
a = [| 5 |]
