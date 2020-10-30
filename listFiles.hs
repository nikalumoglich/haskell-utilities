import Data.List
import System.Directory

main = do
  all <- getDirectoryContents "./"
  let filtered = filter (isSuffixOf ".hs") all
  print filtered