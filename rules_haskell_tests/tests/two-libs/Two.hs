module Two (two) where

import One (one)

two :: Int
two = 
    if True then
        one + one
    else 
        if True then
            1
        else
            2
    