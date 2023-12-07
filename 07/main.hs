solve :: String -> Int
solve = read

main = do
  block <- getContents
  print $ solve block
