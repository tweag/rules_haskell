module Foo (hscFiredFoo) where

#if __GLASGOW_HASKELL__ >= 700
#ifndef _INTERNAL_HSC_DO_NOT_DEFINE_ME
hscFiredFoo :: String
hscFiredFoo = "hscFiredFoo"
#endif
#endif
