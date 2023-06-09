{-# OPTIONS_GHC -Wno-missing-fields #-}

module QQ where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

qq :: QuasiQuoter
qq = QuasiQuoter { quoteExp = stringE }
