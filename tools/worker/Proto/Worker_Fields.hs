{- This file was auto-generated from worker.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds,
  BangPatterns, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.Worker_Fields where
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens
       as Data.ProtoLens
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes
       as Data.ProtoLens.Encoding.Bytes
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing
       as Data.ProtoLens.Encoding.Growing
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe
       as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire
       as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field
       as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum
       as Data.ProtoLens.Message.Enum
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types
       as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2
       as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked
       as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString
       as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8
       as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding
       as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic
       as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed
       as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read

arguments ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "arguments" a) =>
            Lens.Family2.LensLike' f s a
arguments = Data.ProtoLens.Field.field @"arguments"
digest ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "digest" a) =>
         Lens.Family2.LensLike' f s a
digest = Data.ProtoLens.Field.field @"digest"
exitCode ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "exitCode" a) =>
           Lens.Family2.LensLike' f s a
exitCode = Data.ProtoLens.Field.field @"exitCode"
inputs ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "inputs" a) =>
         Lens.Family2.LensLike' f s a
inputs = Data.ProtoLens.Field.field @"inputs"
output ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "output" a) =>
         Lens.Family2.LensLike' f s a
output = Data.ProtoLens.Field.field @"output"
path ::
     forall f s a .
       (Prelude.Functor f, Data.ProtoLens.Field.HasField s "path" a) =>
       Lens.Family2.LensLike' f s a
path = Data.ProtoLens.Field.field @"path"
vec'arguments ::
              forall f s a .
                (Prelude.Functor f,
                 Data.ProtoLens.Field.HasField s "vec'arguments" a) =>
                Lens.Family2.LensLike' f s a
vec'arguments = Data.ProtoLens.Field.field @"vec'arguments"
vec'inputs ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "vec'inputs" a) =>
             Lens.Family2.LensLike' f s a
vec'inputs = Data.ProtoLens.Field.field @"vec'inputs"