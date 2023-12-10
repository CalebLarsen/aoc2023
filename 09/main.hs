import Text.ParserCombinators.Parsec
import Data.Either

solve :: String -> Int
solve s = sum vs
  where vs = map findPrev ls
        ls = fromRight [[]] p
        p = parse pFile "" s

findNext :: [Int] -> Int
findNext xs
  | onlyZero xs = 0
  | otherwise = last xs + findNext (diffs xs)

findPrev :: [Int] -> Int
findPrev xs
  | onlyZero xs = 0
  | otherwise = head xs - findPrev (diffs xs)

onlyZero :: [Int] -> Bool
onlyZero = foldr (\ x -> (&&) (x == 0)) True

diffs :: [Int] -> [Int]
diffs (a:b:xs) = (b-a):diffs (b:xs)
diffs _ = []

pNum :: GenParser Char st Int
pNum = read <$> many (oneOf "-0123456789")

pLine = do
  nums <- pNum `sepBy` char ' '
  newline
  return nums

pFile = do
  ls <- many pLine
  eof
  return ls

main = do
  block <- getContents
  print $ solve block
