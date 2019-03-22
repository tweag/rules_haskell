module TH (foo) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)

foo :: Q Exp
foo = runIO (readFile "tests/encoding/unicode.txt") >>= lift
