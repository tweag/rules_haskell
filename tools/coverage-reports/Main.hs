{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)

import Control.Arrow.ListArrow (runLA)
import Data.Either.Utils (maybeToEither)
import Data.List (find)
import Data.List.Safe (head, tail)
import Data.List.Utils (split)
import Data.Tree.NTree.TypeDefs (NTree(..))
import Prelude hiding (head, tail)
import System.Console.CmdArgs.Implicit (Data, Typeable, cmdArgs)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (FilePath, (</>), takeDirectory)
import qualified Text.XML.HXT.Arrow.ReadDocument as XML
import Text.XML.HXT.DOM.QualifiedName (localPart)
import Text.XML.HXT.DOM.TypeDefs (XNode(..), XmlTree)
import Text.XML.HXT.XPath.XPathEval (getXPath, getXPathSubTrees)

data Args = Args
  { testlog :: FilePath
  , destdir :: FilePath
  } deriving (Data, Typeable)

data ReportFile = ReportFile
  { content :: String
  , filename :: FilePath
  } deriving (Show)

main :: IO ()
main = do
  Args {testlog, destdir} <- cmdArgs $ Args {testlog = "", destdir = ""}
  if testlog == ""
    then putStrLn noTestlogError >> exitFailure
    else do
      fileContents <- readFile testlog
      let xmlTrees = runLA XML.xreadDoc fileContents
      let rootTree = find isRoot xmlTrees
      case rootTree of
        Nothing -> do
          putStrLn "Invalid XML format for testlog."
          exitFailure
        Just tree -> do
          let reportFiles = generateReportFiles tree
          case reportFiles of
            Right reports ->
              forM_ reports $ \ReportFile {content, filename} -> do
                putStrLn $ concat ["Creating ", show $ destdir </> filename]
                createDirectoryIfMissing
                  True
                  (destdir </> takeDirectory filename)
                writeFile (destdir </> filename) content
            Left err -> do
              putStrLn err
              exitFailure

generateReportFiles :: XmlTree -> Either String [ReportFile]
generateReportFiles doc =
  let testSuites = getXPath "/testsuites/testsuite" doc
   in concat <$> sequence (reportsForTestCase <$> testSuites)

reportsForTestCase :: XmlTree -> Either String [ReportFile]
reportsForTestCase testSuite = do
  caseName <-
    extractAttr =<<
    maybeToEither
      "Couldn't find testcase name."
      (head (getXPathSubTrees "/testsuite/testcase/@name" testSuite))
  let coverageOutputDirectory = takeDirectory caseName
  testOutput <-
    extractText =<<
    maybeToEither
      "Couldn't find system output."
      (head (getXPathSubTrees "/testsuite/system-out" testSuite))
  htmlPortion <-
    maybeToEither
      ("Couldn't find HTML report section in test case " ++ caseName ++ ".")
      (head =<< tail (split testOutputSeparator testOutput))
  let coverageReportPartXmlTrees = runLA XML.hreadDoc htmlPortion
  traverse
    (coveragePartToReportFile coverageOutputDirectory)
    coverageReportPartXmlTrees

coveragePartToReportFile :: FilePath -> XmlTree -> Either String ReportFile
coveragePartToReportFile parentDirectory reportPart = do
  filename <-
    extractAttr =<<
    maybeToEither
      "Couldn't find report part name."
      (head (getXPathSubTrees "/coverage-report-part/@name" reportPart))
  content <- extractText reportPart
  return $
    ReportFile
      { content = content
      , filename = "coverage-reports" </> parentDirectory </> filename
      }

noTestlogError :: String
noTestlogError =
  unlines
    [ "ERROR: You must specify the testlog XML file location with --testlog."
    , "It is found inside the bazel-testlog, in the respective"
    , "folder for the test you're interested in."
    , "This must be after having run 'bazel coverage'."
    ]

isRoot :: XmlTree -> Bool
isRoot tree =
  case tree of
    NTree (XTag name _) _ -> localPart name == "testsuites"
    _ -> False

extractAttr :: XmlTree -> Either String String
extractAttr tree =
  case tree of
    NTree (XAttr _) [NTree (XText value) []] -> pure value
    _ -> Left "Couldn't extract attribute from test XML."

extractText :: XmlTree -> Either String String
extractText tree =
  let treeToText :: XmlTree -> String -> String
      treeToText textTree acc =
        case textTree of
          (NTree (XText value) _) -> acc ++ value
          _ -> ""
   in case tree of
        NTree (XTag _ _) textTree -> pure $ foldr treeToText "" textTree
        _ -> Left "Couldn't extract text from test XML."

testOutputSeparator :: String
testOutputSeparator = "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
