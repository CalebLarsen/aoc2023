{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
import Data.List.Split
import Data.Char
import Data.Array
import Data.List

splitChunk = splitOn "\n\n"
splitLines s = filter (not . null) $ splitOn "\n" s
solve :: String -> Int
solve s = sum $ map (gearVals . neighborNums m) stars
  where m = blockToMatrix s
        stars = findStars m

blockToMatrix :: String -> Matrix
blockToMatrix s = array (0, len) (zip [0..len] (map lineToArray lines))
  where lines = splitLines s
        len = length lines - 1

lineToArray :: String -> Array Int Char
lineToArray s = array (0, length s - 1) (zip [0..length s - 1] s)

type Matrix = Array Int (Array Int Char)

at :: Matrix -> Pos -> Char
at m (x, y) = if x >= minX && x <= maxX && y >= minY && y <= maxY then (m!y)!x else '.'
  where minY = fst (bounds m)
        maxY = snd (bounds m)
        minX = fst (bounds (m!0))
        maxX = snd (bounds (m!0))

neighbors :: Matrix -> Pos -> [Pos]
neighbors m (x, y) = [
  (x-1, y-1), (x, y-1), (x+1, y-1),
  (x-1,   y),           (x+1,   y),
  (x-1, y+1), (x, y+1), (x+1, y+1)]

type Pos = (Int, Int)

findStarsLine :: Matrix -> Pos -> [Pos]
findStarsLine m (x, y)
  | x > len = []
  | at m (x, y) == '*' = (x, y):findStarsLine m (x+1, y)
  | otherwise = findStarsLine m (x+1, y)
  where
      len = length m

findStars :: Matrix -> [Pos]
findStars m = concatMap (\y -> findStarsLine m (0, y)) [0..len]
  where len = length m

getNumL :: Matrix -> Pos -> Int
getNumL m (x, y)
  | isDigit currC = prev*10 + read [currC]
  | otherwise = 0
  where prev = getNumL m (x-1, y)
        currC = at m (x,y)

getNumR :: Matrix -> Pos -> Int -> Int
getNumR m (x, y) val
  | isDigit currC =
      getNumR m (x+1, y) (val*10 + read [currC])
  | otherwise = val
  where currC = at m (x,y)

getNum :: Matrix -> Pos -> Int
getNum m (x, y)
  | not $ isDigit (at m (x, y)) = 0
  | otherwise = getNumR m (x+1, y) (getNumL m (x, y))

neighborNums :: Matrix -> Pos -> [Int]
neighborNums m (x, y) = nub (filter (> 0) (map (getNum m ) $ neighbors m (x,y)))

gearVals :: [Int] -> Int
gearVals xs
  | length xs == 2 = product xs
  | otherwise = 0

main = do
  block <- getContents
  print $ solve block
