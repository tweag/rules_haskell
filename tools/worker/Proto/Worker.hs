{- This file was auto-generated from worker.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds,
  BangPatterns, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.Worker (Input(), WorkRequest(), WorkResponse()) where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq
       as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism
       as Data.ProtoLens.Prism
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

{- | Fields :

    * 'Proto.Worker_Fields.path' @:: Lens' Input Data.Text.Text@
    * 'Proto.Worker_Fields.digest' @:: Lens' Input Data.ByteString.ByteString@
 -}
data Input = Input{_Input'path :: !Data.Text.Text,
                   _Input'digest :: !Data.ByteString.ByteString,
                   _Input'_unknownFields :: !Data.ProtoLens.FieldSet}
               deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Input where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Input "path"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Input'path
               (\ x__ y__ -> x__{_Input'path = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField Input "digest"
           (Data.ByteString.ByteString)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _Input'digest
               (\ x__ y__ -> x__{_Input'digest = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message Input where
        messageName _ = Data.Text.pack "blaze.worker.Input"
        fieldsByTag
          = let path__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "path"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"path"))
                      :: Data.ProtoLens.FieldDescriptor Input
                digest__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "digest"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"digest"))
                      :: Data.ProtoLens.FieldDescriptor Input
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, path__field_descriptor),
                 (Data.ProtoLens.Tag 2, digest__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Input'_unknownFields
              (\ x__ y__ -> x__{_Input'_unknownFields = y__})
        defMessage
          = Input{_Input'path = Data.ProtoLens.fieldDefault,
                  _Input'digest = Data.ProtoLens.fieldDefault,
                  _Input'_unknownFields = ([])}
        parseMessage
          = let loop :: Input -> Data.ProtoLens.Encoding.Bytes.Parser Input
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "path"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"path") y
                                              x)
                                18 -> do y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                  Data.ProtoLens.Encoding.Bytes.getBytes
                                                    (Prelude.fromIntegral len))
                                                Data.ProtoLens.Encoding.Bytes.<?> "digest"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"digest")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "Input"
        buildMessage
          = (\ _x ->
               (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"path") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 10) Data.Monoid.<>
                      (((\ bs ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                             Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Prelude.. Data.Text.Encoding.encodeUtf8)
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"digest") _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 18) Data.Monoid.<>
                        (\ bs ->
                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                             Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs)
                          _v)
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Input where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_Input'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_Input'path x__)
                    (Control.DeepSeq.deepseq (_Input'digest x__) (()))))
{- | Fields :

    * 'Proto.Worker_Fields.arguments' @:: Lens' WorkRequest [Data.Text.Text]@
    * 'Proto.Worker_Fields.vec'arguments' @:: Lens' WorkRequest (Data.Vector.Vector Data.Text.Text)@
    * 'Proto.Worker_Fields.inputs' @:: Lens' WorkRequest [Input]@
    * 'Proto.Worker_Fields.vec'inputs' @:: Lens' WorkRequest (Data.Vector.Vector Input)@
 -}
data WorkRequest = WorkRequest{_WorkRequest'arguments ::
                               !(Data.Vector.Vector Data.Text.Text),
                               _WorkRequest'inputs :: !(Data.Vector.Vector Input),
                               _WorkRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
                     deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show WorkRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField WorkRequest "arguments"
           ([Data.Text.Text])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _WorkRequest'arguments
               (\ x__ y__ -> x__{_WorkRequest'arguments = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField WorkRequest "vec'arguments"
           (Data.Vector.Vector Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _WorkRequest'arguments
               (\ x__ y__ -> x__{_WorkRequest'arguments = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField WorkRequest "inputs"
           ([Input])
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _WorkRequest'inputs
               (\ x__ y__ -> x__{_WorkRequest'inputs = y__}))
              Prelude..
              Lens.Family2.Unchecked.lens Data.Vector.Generic.toList
                (\ _ y__ -> Data.Vector.Generic.fromList y__)
instance Data.ProtoLens.Field.HasField WorkRequest "vec'inputs"
           (Data.Vector.Vector Input)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _WorkRequest'inputs
               (\ x__ y__ -> x__{_WorkRequest'inputs = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message WorkRequest where
        messageName _ = Data.Text.pack "blaze.worker.WorkRequest"
        fieldsByTag
          = let arguments__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "arguments"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"arguments"))
                      :: Data.ProtoLens.FieldDescriptor WorkRequest
                inputs__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "inputs"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Input)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Data.ProtoLens.Field.field @"inputs"))
                      :: Data.ProtoLens.FieldDescriptor WorkRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, arguments__field_descriptor),
                 (Data.ProtoLens.Tag 2, inputs__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _WorkRequest'_unknownFields
              (\ x__ y__ -> x__{_WorkRequest'_unknownFields = y__})
        defMessage
          = WorkRequest{_WorkRequest'arguments = Data.Vector.Generic.empty,
                        _WorkRequest'inputs = Data.Vector.Generic.empty,
                        _WorkRequest'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     WorkRequest ->
                       Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                         Data.ProtoLens.Encoding.Growing.RealWorld
                         Data.Text.Text
                         ->
                         Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector
                           Data.ProtoLens.Encoding.Growing.RealWorld
                           Input
                           -> Data.ProtoLens.Encoding.Bytes.Parser WorkRequest
                loop x mutable'arguments mutable'inputs
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do frozen'arguments <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                  (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                     mutable'arguments)
                            frozen'inputs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                               (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                  mutable'inputs)
                            let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'arguments")
                                    frozen'arguments
                                    (Lens.Family2.set (Data.ProtoLens.Field.field @"vec'inputs")
                                       frozen'inputs
                                       x)))
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                10 -> do !y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                               Data.ProtoLens.Encoding.Bytes.getBytes
                                                                 (Prelude.fromIntegral len)
                                                   Data.ProtoLens.Encoding.Bytes.runEither
                                                     (case Data.Text.Encoding.decodeUtf8' value of
                                                          Prelude.Left err -> Prelude.Left
                                                                                (Prelude.show err)
                                                          Prelude.Right r -> Prelude.Right r))
                                                 Data.ProtoLens.Encoding.Bytes.<?> "arguments"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append
                                                   mutable'arguments
                                                   y)
                                         loop x v mutable'inputs
                                18 -> do !y <- (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                   Data.ProtoLens.Encoding.Bytes.isolate
                                                     (Prelude.fromIntegral len)
                                                     Data.ProtoLens.parseMessage)
                                                 Data.ProtoLens.Encoding.Bytes.<?> "inputs"
                                         v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.append
                                                   mutable'inputs
                                                   y)
                                         loop x mutable'arguments v
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
                                             mutable'arguments
                                             mutable'inputs
              in
              (do mutable'arguments <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         Data.ProtoLens.Encoding.Growing.new
                  mutable'inputs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      Data.ProtoLens.Encoding.Growing.new
                  loop Data.ProtoLens.defMessage mutable'arguments mutable'inputs)
                Data.ProtoLens.Encoding.Bytes.<?> "WorkRequest"
        buildMessage
          = (\ _x ->
               (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                  (\ _v ->
                     (Data.ProtoLens.Encoding.Bytes.putVarInt 10) Data.Monoid.<>
                       (((\ bs ->
                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                              Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Prelude.. Data.Text.Encoding.encodeUtf8)
                         _v)
                  (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'arguments")
                     _x))
                 Data.Monoid.<>
                 (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                    (\ _v ->
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 18) Data.Monoid.<>
                         (((\ bs ->
                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Prelude.. Data.ProtoLens.encodeMessage)
                           _v)
                    (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'inputs") _x))
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData WorkRequest where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_WorkRequest'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_WorkRequest'arguments x__)
                    (Control.DeepSeq.deepseq (_WorkRequest'inputs x__) (()))))
{- | Fields :

    * 'Proto.Worker_Fields.exitCode' @:: Lens' WorkResponse Data.Int.Int32@
    * 'Proto.Worker_Fields.output' @:: Lens' WorkResponse Data.Text.Text@
 -}
data WorkResponse = WorkResponse{_WorkResponse'exitCode ::
                                 !Data.Int.Int32,
                                 _WorkResponse'output :: !Data.Text.Text,
                                 _WorkResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
                      deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show WorkResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField WorkResponse "exitCode"
           (Data.Int.Int32)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _WorkResponse'exitCode
               (\ x__ y__ -> x__{_WorkResponse'exitCode = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Field.HasField WorkResponse "output"
           (Data.Text.Text)
         where
        fieldOf _
          = (Lens.Family2.Unchecked.lens _WorkResponse'output
               (\ x__ y__ -> x__{_WorkResponse'output = y__}))
              Prelude.. Prelude.id
instance Data.ProtoLens.Message WorkResponse where
        messageName _ = Data.Text.pack "blaze.worker.WorkResponse"
        fieldsByTag
          = let exitCode__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "exit_code"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"exitCode"))
                      :: Data.ProtoLens.FieldDescriptor WorkResponse
                output__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "output"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Data.ProtoLens.Field.field @"output"))
                      :: Data.ProtoLens.FieldDescriptor WorkResponse
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, exitCode__field_descriptor),
                 (Data.ProtoLens.Tag 2, output__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _WorkResponse'_unknownFields
              (\ x__ y__ -> x__{_WorkResponse'_unknownFields = y__})
        defMessage
          = WorkResponse{_WorkResponse'exitCode =
                           Data.ProtoLens.fieldDefault,
                         _WorkResponse'output = Data.ProtoLens.fieldDefault,
                         _WorkResponse'_unknownFields = ([])}
        parseMessage
          = let loop ::
                     WorkResponse -> Data.ProtoLens.Encoding.Bytes.Parser WorkResponse
                loop x
                  = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
                       if end then
                         do let missing = [] in
                              if Prelude.null missing then Prelude.return () else
                                Prelude.fail
                                  (("Missing required fields: ") Prelude.++
                                     Prelude.show (missing :: ([Prelude.String])))
                            Prelude.return
                              (Lens.Family2.over Data.ProtoLens.unknownFields
                                 (\ !t -> Prelude.reverse t)
                                 x)
                         else
                         do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                            case tag of
                                8 -> do y <- (Prelude.fmap Prelude.fromIntegral
                                                Data.ProtoLens.Encoding.Bytes.getVarInt)
                                               Data.ProtoLens.Encoding.Bytes.<?> "exit_code"
                                        loop
                                          (Lens.Family2.set (Data.ProtoLens.Field.field @"exitCode")
                                             y
                                             x)
                                18 -> do y <- (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                              Data.ProtoLens.Encoding.Bytes.getBytes
                                                                (Prelude.fromIntegral len)
                                                  Data.ProtoLens.Encoding.Bytes.runEither
                                                    (case Data.Text.Encoding.decodeUtf8' value of
                                                         Prelude.Left err -> Prelude.Left
                                                                               (Prelude.show err)
                                                         Prelude.Right r -> Prelude.Right r))
                                                Data.ProtoLens.Encoding.Bytes.<?> "output"
                                         loop
                                           (Lens.Family2.set (Data.ProtoLens.Field.field @"output")
                                              y
                                              x)
                                wire -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                                   wire
                                           loop
                                             (Lens.Family2.over Data.ProtoLens.unknownFields
                                                (\ !t -> (:) y t)
                                                x)
              in
              (do loop Data.ProtoLens.defMessage)
                Data.ProtoLens.Encoding.Bytes.<?> "WorkResponse"
        buildMessage
          = (\ _x ->
               (let _v
                      = Lens.Family2.view (Data.ProtoLens.Field.field @"exitCode") _x
                  in
                  if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty else
                    (Data.ProtoLens.Encoding.Bytes.putVarInt 8) Data.Monoid.<>
                      ((Data.ProtoLens.Encoding.Bytes.putVarInt) Prelude..
                         Prelude.fromIntegral)
                        _v)
                 Data.Monoid.<>
                 (let _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"output") _x
                    in
                    if (_v) Prelude.== Data.ProtoLens.fieldDefault then
                      Data.Monoid.mempty else
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 18) Data.Monoid.<>
                        (((\ bs ->
                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                (Prelude.fromIntegral (Data.ByteString.length bs)))
                               Data.Monoid.<> Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Prelude.. Data.Text.Encoding.encodeUtf8)
                          _v)
                   Data.Monoid.<>
                   Data.ProtoLens.Encoding.Wire.buildFieldSet
                     (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData WorkResponse where
        rnf
          = (\ x__ ->
               Control.DeepSeq.deepseq (_WorkResponse'_unknownFields x__)
                 (Control.DeepSeq.deepseq (_WorkResponse'exitCode x__)
                    (Control.DeepSeq.deepseq (_WorkResponse'output x__) (()))))