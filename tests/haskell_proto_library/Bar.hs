module Bar (bar) where

import Proto.StrippedAddress
import Proto.Tests.HaskellProtoLibrary.Person
import Proto.Tests.HaskellProtoLibrary.Person_Fields

import Data.ProtoLens.Message

defPerson :: Person
defPerson = defMessage

bar :: Int
bar = 5
