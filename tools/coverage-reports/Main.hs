{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)

import Control.Arrow.ListArrow (runLA)
import Data.List (find)
import Data.List.Safe ((!!), head, tail)
import Data.List.Utils (replace, split)
import qualified Data.Maybe as Maybe
import Data.Tree.NTree.TypeDefs (NTree(..))
import Prelude hiding ((!!), head, tail)
import System.Console.CmdArgs.Implicit (Data, Typeable, cmdArgs)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath (FilePath, (</>), takeDirectory)
import qualified Text.XML.HXT.Arrow.ReadDocument as XML
import Text.XML.HXT.Arrow.WriteDocument (writeDocumentToString)
import Text.XML.HXT.DOM.QualifiedName (localPart, mkName)
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
          forM_ reportFiles $ \ReportFile {content, filename} -> do
            putStrLn filename
            createDirectoryIfMissing True (destdir </> takeDirectory filename)
            writeFile (destdir </> filename) content

generateReportFiles :: XmlTree -> [ReportFile]
generateReportFiles doc =
  let testSuites = getXPath "/testsuites/testsuite" doc
   in concat $ Maybe.catMaybes $ reportsForTestCase <$> testSuites

reportsForTestCase :: XmlTree -> Maybe [ReportFile]
reportsForTestCase testSuite = do
  caseName <-
    extractAttr =<<
    head (getXPathSubTrees "/testsuite/testcase/@name" testSuite)
  let coverageOutputDirectory = takeDirectory caseName
  testOutput <-
    extractText =<< head (getXPathSubTrees "/testsuite/system-out" testSuite)
  htmlPortion <- head =<< tail (split testOutputSeparator testOutput)
  let coverageReportPartXmlTrees = runLA XML.hreadDoc htmlPortion
  traverse
    (coveragePartToReportFile coverageOutputDirectory)
    coverageReportPartXmlTrees

coveragePartToReportFile :: FilePath -> XmlTree -> Maybe ReportFile
coveragePartToReportFile parentDirectory reportPart = do
  filename <-
    extractAttr =<<
    head (getXPathSubTrees "/coverage-report-part/@name" reportPart)
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

extractAttr :: XmlTree -> Maybe String
extractAttr tree =
  case tree of
    NTree (XAttr _) [NTree (XText value) []] -> pure value
    _ -> Nothing

extractText :: XmlTree -> Maybe String
extractText tree =
  let treeToText :: XmlTree -> String -> String
      treeToText textTree acc =
        case textTree of
          (NTree (XText value) _) -> value
          _ -> ""
   in case tree of
        NTree (XTag _ _) textTree -> pure $ foldr treeToText "" textTree
        _ -> Nothing

testOutputSeparator :: String
testOutputSeparator =
  "\n-----------------------------------------------------------------------------\n"
