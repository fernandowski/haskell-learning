module RecursiveContents (getRecursiveContents) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topDir = do
  names <- getDirectoryContents topDir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames (properNamesFunction topDir)
  return (concat paths)

properNamesFunction topDir name = do
  let path = topDir </> name
  isDirectory <- doesDirectoryExist path
  if isDirectory then getRecursiveContents path
  else return [path]

