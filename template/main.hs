import Data.List.Split

splitChunk = splitOn "\n\n"
splitLines = splitOn "\n"
solve :: String -> Integer
solve = read

main = do
  block <- getContents
  print $ solve block
