module Lib (libText) where

import Lib.MsgType (Msg)
import MessageSuffix (messageSuffix)

libText :: Msg
libText = "hello " ++ messageSuffix
