{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cat
  ( -- * I/O Interface
    runCat
    -- * Conduits
  , catSource
  , fileSource
  , numberLinesUtf8
  , numberLines
  ) where

import Args (Args)
import qualified Args
import Conduit
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Conduit.List (mapAccum)
import Data.Conduit.Text (decodeUtf8Lenient, encodeUtf8, lines)
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude hiding (lines)
import TextShow (showt)


-- | Read the inputs and write them to standard out.
runCat :: Args -> IO ()
runCat args = runConduitRes (catSource args .| stdoutC)


-- | A source for the files defined by the given arguments,
-- numbered or unnumbered as defined by the given arguments.
catSource
  :: MonadResource m
  => Args -> ConduitT i ByteString m ()
catSource args
  | Args.number args = unnumbered .| numberLinesUtf8
  | otherwise = unnumbered
  where
    unnumbered = yieldMany (Args.files args) .| fileSource


-- | Open a source for a 'Args.FileArg' argument.
fileSource
  :: MonadResource m
  => ConduitT Args.FileArg ByteString m ()
fileSource = awaitForever $ \case
  -- XXX: Annotate file IO exceptions with file-name.
  Args.StdIn -> stdinC
  Args.File f -> sourceFile f
{-# INLINE fileSource #-}


-- | Number lines of the input stream, assuming that they are UTF-8 encoded.
numberLinesUtf8
  :: Monad m
  => ConduitT ByteString ByteString m ()
numberLinesUtf8 = decodeUtf8Lenient .| numberLines .| encodeUtf8


-- | Prepend each line with its number starting from one.
--
-- Note, that this diverges from cat's behaviour, in that resulting lines will
-- always end in a new line, even if the input didn't.
numberLines
  :: forall m. Monad m
  => ConduitT Text Text m ()
numberLines = void $ lines .| mapAccum go 1
  where
    go :: Text -> Int -> (Int, Text)
    go l n = (succ n, numbered)
      where
        numbered =
          Text.concat
            [ Text.justifyRight width ' ' (showt n)
            , Text.replicate gap " "
            , l
            , "\n"
            ]
    width :: Int
    width = 6
    gap :: Int
    gap = 2

-- An alternative implementation that seems to be a fair bit slower.
--
--   numberLines = void $ foldLines go 1
--     where
--       go :: Int -> ConduitT Text Text m Int
--       go n = do
--         yield $ Text.justifyRight width ' ' (showt n)
--         yield $ Text.replicate gap " "
--         awaitForever yield
--         yield "\n"
--         pure (succ n)
