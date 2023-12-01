import Data.List.Split
import Data.List
import Data.Char

splitChunk = splitOn "\n\n"
splitLines s = filter (not . null) $ splitOn "\n" s

halves :: String -> [String]
halves s = chunksOf (div (length s) 2) s

threes :: [String] -> [[String]]
threes = chunksOf 3

unwrap :: Maybe [a] -> [a]
unwrap (Just a) = a
unwrap _ = []
first :: [[a]] -> [a]
first l = unwrap $ find (const True) l
com s = nub $ foldr intersect (first s) s

val c = if isLower c then ord c - ord 'a' + 1 else ord c - ord 'A' + 1 + 26

p1 :: String -> [[Char]]
p1 s = map com $ threes $ splitLines s

p2 :: [[Char]] -> Int
p2 xs = sum $ map (sum . map val) xs

solve :: String -> Int
solve s = p2 $ p1 s

main = do
  block <- getContents
  --print $ map halves $ splitLines block
  print $ solve block
