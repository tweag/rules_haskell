module Lib where
import Paths_lib
import System.Environment
import Data.List
import System.FilePath

writeCabalPathsValues :: String -> IO ()
writeCabalPathsValues outputFileName = do
  bindir <- getBinDir   
  libdir <- getLibDir     
  dynlibdir <- getDynLibDir  
  libexecdir <- getLibexecDir 
  sysconfdir <- getSysconfDir 
  exePath <- getExecutablePath
  let (dir,_) = splitFileName exePath
  let paths = map (makeRelative dir) [bindir, libdir, dynlibdir, libexecdir, sysconfdir]
  let v = version
  writeFile outputFileName $ intercalate "\n"
      (paths ++ [show v, ""])

