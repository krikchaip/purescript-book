module FileOperations where

import Prelude (bind, otherwise, pure, (==), (>), (>>>))
import Data.Array
import Data.Path (Path, filename, isDirectory, ls, size)
import Data.Maybe (Maybe(..))

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

-- easy
onlyFiles :: Path -> Array Path
onlyFiles = ls >>> concatMap getFiles where
  getFiles item
    | isDirectory item = onlyFiles item
    | otherwise = [item]

-- medium
largest :: Path -> Maybe Path
largest = onlyFiles >>> getMax where
  getMax = foldr
    (\file max -> case max of
      Just m -> do
        a <- size m
        b <- size file
        if b > a then pure file else pure m
      Nothing -> Just file)
    Nothing

-- difficult
whereIs :: String -> Path -> Maybe Path
whereIs file fs = head (search fs) where
  search dir = do
    item <- ls dir
    if isDirectory item
      then search item
      else if filename item == file
        then pure dir
        else []