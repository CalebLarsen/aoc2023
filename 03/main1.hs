{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
import Data.List.Split
import Data.Char
import Data.Array

splitChunk = splitOn "\n\n"
splitLines s = filter (not . null) $ splitOn "\n" s
solve :: String -> Int
solve s = sumNBlock m s
  where m = blockToMatrix s

blockToMatrix :: String -> Matrix
blockToMatrix s = array (0, len) (zip [0..len] (map lineToArray lines))
  where lines = splitLines s
        len = length lines - 1

lineToArray :: String -> Array Int Char
lineToArray s = array (0, length s - 1) (zip [0..length s - 1] s)

sumBlock :: String -> Int
sumBlock s = sum $ map (sumLine 0 0) (splitLines s)

sumLine :: Int -> Int -> String -> Int
sumLine currNumber runningTotal (c:cs)
  | isDigit c = sumLine (currNumber*10+read [c]) runningTotal cs
  | otherwise = sumLine 0 (runningTotal+currNumber) cs
sumLine currNumber runningTotal [] = runningTotal+currNumber

type Matrix = Array Int (Array Int Char)

at :: Matrix -> Int -> Int -> Char
at m x y = if x >= minX && x <= maxX && y >= minY && y <= maxY then (m!y)!x else '.'
  where minY = fst (bounds m)
        maxY = snd (bounds m)
        minX = fst (bounds (m!0))
        maxX = snd (bounds (m!0))

isSymb :: Char -> Bool
isSymb c = not (isDigit c) && (c /= '.')

neighbors :: Matrix -> Int -> Int -> [Char]
neighbors m x y = [
  at m (x-1) (y-1), at m x (y-1), at m (x+1) (y-1),
  at m (x-1)     y,               at m (x+1)     y,
  at m (x-1) (y+1), at m x (y+1), at m (x+1) (y+1)]

symbolNeighbors :: Matrix -> Int -> Int -> Bool
symbolNeighbors m x y = any isSymb $ neighbors m x y

data Pos =
  Pos {m :: Matrix, x :: Int, y :: Int}

sumNBlock :: Matrix -> String -> Int
sumNBlock m s = sum $ zipWith (curry (\x -> sumNeighborlessLine (Pos m 0 (fst x)) 0 0 False (snd x))) [0..len] lines
  where lines = splitLines s
        len = length lines - 1

sumNeighborlessLine :: Pos -> Int -> Int -> Bool -> String -> Int
sumNeighborlessLine (Pos m x y) currNumber runningTotal hasNeighbor (c:cs)
  | isDigit c = sumNeighborlessLine (Pos m (x+1) y) (currNumber*10+read [c]) runningTotal (hasNeighbor || symbolNeighbors m x y) cs
  | hasNeighbor = sumNeighborlessLine (Pos m (x+1) y) 0 (runningTotal+currNumber) False cs
  | otherwise = sumNeighborlessLine (Pos m (x+1) y) 0 runningTotal False cs
sumNeighborlessLine (Pos m x y) currNumber runningTotal hasNeighbor []
  | hasNeighbor = runningTotal+currNumber
  | otherwise = runningTotal

main = do
  block <- getContents
  print $ solve block
