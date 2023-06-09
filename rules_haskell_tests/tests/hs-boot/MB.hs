module MB where

import {-# SOURCE #-} MA (TA (..))

data TB = MkTB !Int

g :: TA -> TB
g (MkTA x) = MkTB x
