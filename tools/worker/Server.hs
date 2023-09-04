{-# LANGUAGE OverloadedStrings, NondecreasingIndentation #-}

module Server (server) where

{- ProtoBuf -}
import qualified Proto.Worker as W
import qualified Proto.Worker_Fields as W
import Data.ProtoLens (defMessage, pprintMessage)
import Data.ProtoLens.Encoding
  ( buildMessageDelimited
  , decodeMessageDelimitedH
  )
import Data.ProtoLens.Encoding.Bytes ( runBuilder )
import Lens.Micro

import Control.Monad
import qualified Data.ByteString as S
import qualified Data.Text as T
import System.IO

import Compile (compile)

server :: Handle -> Handle -> [String] -> IO ()
server hIn hOut extra_args = do
    hSetBuffering stderr NoBuffering
    hSetBuffering hIn NoBuffering
    hSetBuffering hOut  NoBuffering
    hSetBinaryMode hIn True
    hSetBinaryMode hOut True
    hPutStrLn stderr "Server Starts"
    _ <- forever loop
    hPutStrLn stderr "Server: Bye"
  where
    loop = do
      let logH = stderr

      hPutStrLn logH $ "Server: Waiting for a message..."
      msg <- decodeMessageDelimitedH hIn

      hPutStrLn logH "Server: got a message, decoding..."
      req <- either fail return msg :: IO W.WorkRequest
      hPutStrLn logH $ "Server: msg received:\n" ++ (show . pprintMessage $ req)

      -- Processing a request
      resp <- processRequest req extra_args

      let msgresp = runBuilder . buildMessageDelimited $ resp
      S.hPut hOut msgresp
      hPutStrLn logH $ "Server sent response..."

processRequest :: W.WorkRequest -> [String] -> IO W.WorkResponse
processRequest req extra_args = do
    let (args, _inputs) = destructRequest req

    compile (args ++ extra_args)

    return sampleResponse

destructRequest :: W.WorkRequest -> ([String], [String])
destructRequest req = (flags, inputs)
  where
    flags = map T.unpack $ req ^. W.arguments
    inputs = map (T.unpack . (^. W.path)) $ req ^. W.inputs

sampleResponse :: W.WorkResponse
sampleResponse = defMessage
    & W.exitCode .~ 0
    & W.output   .~ "All good.\n"
