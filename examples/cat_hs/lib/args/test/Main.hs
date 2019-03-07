module Main
  ( main
  ) where

import Args (Args (Args))
import qualified Args
import Options.Applicative
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Args.parser" $ do
    it "parses no arguments" $ do
      parse []
        `shouldBe`
        Just Args { Args.files = [], Args.number = False }
    it "parses one stdin" $ do
      parse ["-"]
        `shouldBe`
        Just Args { Args.files = [Args.StdIn], Args.number = False }
    it "parses two stdin" $ do
      parse ["-", "-"]
        `shouldBe`
        Just Args
          { Args.files = [Args.StdIn, Args.StdIn], Args.number = False }
    it "parses numbered stdin" $ do
      parse ["-n", "-"]
        `shouldBe`
        Just Args { Args.files = [Args.StdIn], Args.number = True }
    it "parses numbered stdin reversed" $ do
      parse ["-", "-n"]
        `shouldBe`
        Just Args { Args.files = [Args.StdIn], Args.number = True }
    it "parses file -n" $ do
      parse ["--", "-n"]
        `shouldBe`
        Just Args { Args.files = [Args.File "-n"], Args.number = False }
    it "parses file -" $ do
      parse ["./-"]
        `shouldBe`
        Just Args { Args.files = [Args.File "./-"], Args.number = False }
    it "parses stdin and file" $ do
      parse ["-", "file"]
        `shouldBe`
        Just Args
          { Args.files = [Args.StdIn, Args.File "file"], Args.number = False }
    it "recognizes -h" $ do
      parse ["-h"]
        `shouldBe`
        Nothing
    it "recognizes --help" $ do
      parse ["-h"]
        `shouldBe`
        Nothing
    it "parses file -h" $ do
      parse ["--", "-h"]
        `shouldBe`
        Just Args { Args.files = [Args.File "-h"], Args.number = False }


-- | Execute the command-line parser on the given arguments.
-- Returns 'Nothing' if the parser failed, or @--help@ was passed.
parse :: [String] -> Maybe Args
parse args =
  getParseResult $
    execParserPure defaultPrefs Args.parser args
