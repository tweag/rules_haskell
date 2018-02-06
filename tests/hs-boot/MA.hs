module MA where

import MB (TB (..))

newtype TA = MkTA Int

f :: TB -> TA
f (MkTB x) = MkTA x
