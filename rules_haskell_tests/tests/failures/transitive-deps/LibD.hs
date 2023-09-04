module LibD (thingD) where

import LibA (thingA)
import LibB (thingB)

thingD :: Int
thingD = thingA + thingB
