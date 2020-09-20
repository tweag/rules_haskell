module Bar (bar) where

-- TODO this doesn't work as the Haskell packages generated for Protobuf dependencies
-- are hidden by default.
--
-- As a workaround, we wrap FileDescriptorSet in a custom message.
--
-- import qualified Proto.Google.Protobuf.Descriptor

import Proto.Tests.HaskellProtoSimple.Foo (Baz, FileDescriptorSet2)
import Data.ProtoLens.Message (defMessage)

bar :: Baz
bar = defMessage

