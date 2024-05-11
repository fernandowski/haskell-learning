import RecursiveContents (getRecursiveContents)

simpleFind :: (FilePath -> bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
  names <- getRecursiveContents path
  return (filter p names)
