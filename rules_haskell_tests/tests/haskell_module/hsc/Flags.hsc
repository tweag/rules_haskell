module Flags (hscFlags) where

#ifdef THIS_IS_TRUE
#ifdef THIS_TOO_IS_TRUE
hscFlags :: String
hscFlags = "hscFlags"
#endif
#endif
