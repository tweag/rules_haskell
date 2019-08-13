{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Args (Args (Args))
import qualified Args
import Cat
import Conduit
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "numberLines" $ do
    let runNumberLines :: [Text] -> Text.Lazy.Text
        runNumberLines ls =
          runConduitPure $
            yieldMany ls .| numberLines .| sinkLazy
    it "handles empty input" $ do
      runNumberLines []
        `shouldBe`
        ""
    it "handles one line" $
      runNumberLines ["first line"]
        `shouldBe`
        "     1  first line\n"
    it "handles two lines" $
      runNumberLines ["first line\nsecond line"]
        `shouldBe`
        "     1  first line\n     2  second line\n"
    it "handles two lines across chunks" $
      runNumberLines ["first ", "line\nsecond", " line"]
        `shouldBe`
        "     1  first line\n     2  second line\n"
    it "preserves one new line at the end" $
      runNumberLines ["first line\n"]
        `shouldBe`
        "     1  first line\n"
    it "handles empty line within input" $
      runNumberLines ["first line\n\n"]
        `shouldBe`
        "     1  first line\n     2  \n"
