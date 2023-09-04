{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module PreludeShadowing where

import           Data.ByteString

(>>=) :: ByteString -> ByteString -> ByteString
_ >>= _ = "blah"

stdin :: ByteString
stdin = "stdin"
