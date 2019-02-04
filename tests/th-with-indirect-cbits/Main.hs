{-# LANGUAGE TemplateHaskell #-}

import HsLibDirect

main :: IO ()
main = print $(direct)
