module Tox where

foreign import ccall tox_self_get_udp_port :: Int -> IO Int
