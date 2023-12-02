import Data.List.Split

splitChunk = splitOn "\n\n"
splitLines s = filter (not . null) $ splitOn "\n" s
solve :: String -> Int
solve = read

main = do
  block <- getContents
  print $ solve block
