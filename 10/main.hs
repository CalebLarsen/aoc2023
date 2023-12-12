import Text.ParserCombinators.Parsec
import Data.Either
import Data.List

solve :: String -> Int
solve s = maxSteps m
  where m = fromRight [""] p
        p = parse pFile "" s

type Maze = [String]
type MazeData = [[Int]]

data Dir = North | South | East | West deriving (Eq, Enum, Show)
dirs = [North, South, East, West]

at m (x, y) = (m!!y)!!x

setL :: [a] -> Int -> a -> [a]
setL [] _ _ = []
setL (x:xs) 0 v = v:xs
setL (x:xs) i v = x:setL xs (i-1) v

set :: [[a]] -> Int -> Int -> a -> [[a]]
set m x y v = setL m y $ setL (m!!y) x v

connects :: Dir -> Char -> Bool
connects West c
  | c == '-'  = True
  | c == 'J'  = True
  | c == '7'  = True
  | otherwise = False
connects East c
  | c == '-'  = True
  | c == 'L'  = True
  | c == 'F'  = True
  | otherwise = False
connects North c
  | c == '|'  = True
  | c == 'L'  = True
  | c == 'J'  = True
  | otherwise = False
connects South c
  | c == '|'  = True
  | c == '7'  = True
  | c == 'F'  = True
  | otherwise = False

inv :: Dir -> Dir
inv North = South
inv South = North
inv West  = East
inv East  = West

moveDir :: Dir -> (Int, Int) -> (Int, Int)
moveDir West (x, y) = (x-1, y)
moveDir East (x, y) = (x+1, y)
moveDir North (x, y) = (x, y-1)
moveDir South (x, y) = (x, y+1)

walkMaze :: Maze -> MazeData -> Dir -> Int -> (Int, Int) -> MazeData
walkMaze m md prev steps (x, y) =
  if c == 'S' then
    md
  else
    walkMaze m newData (inv next) (steps + 1) nextCoords
  where c    = m `at` (x, y)
        next = head $ filter (prev /=) $ filter (`connects` c) dirs
        nextCoords = moveDir next (x, y)
        newData = set md x y (steps + 1)

findStart :: Maze -> (Int, Int) -> (Int, Int)
findStart m (x, y)
  | x >= lenX = findStart m (0, y+1)
  | m `at` (x, y) == 'S' = (x, y)
  | otherwise = findStart m (x+1, y)
  where
      lenY = length m
      lenX = length (head m)

maxMaze :: MazeData -> Int
maxMaze md = maximum $ map maximum md

foldMd :: MazeData -> MazeData -> MazeData
foldMd = zipWith foldLine

foldLine :: [Int] -> [Int] -> [Int]
foldLine = zipWith min

neighbors :: Maze -> (Int, Int) -> [(Int, Int, Dir, Char)]
neighbors m (x, y) = l4
  where l0 = []
        l1 = if x > 0 then (x-1, y, East, at m (x-1,y)):l0 else l0
        l2 = if x < (length m - 1) then (x+1, y, West, at m (x+1, y)):l1 else l1
        l3 = if y > 0 then (x, y-1, South, at m (x, y-1)):l2 else l2
        l4 = if y < (length (head m) - 1) then (x, y+1, North, at m (x, y+1)):l3 else l3

findWalks :: Maze -> [MazeData]
findWalks m = walks
  where (sX, sY) = findStart m (0, 0)
        neighs = neighbors m (sX, sY)
        opts = filter (\(_, _, d, c) -> connects d c) neighs
        lenY = length m
        lenX = length (head m)
        md = set (replicate lenY $ replicate lenX (negate 1)) sX sY 0
        walks = map (\(x, y, d, _) -> walkMaze m md d 0 (x, y)) opts

maxSteps :: Maze -> Int
maxSteps m = maxMaze $ foldr1 foldMd walks
  where walks = findWalks m

pNum :: GenParser Char st Int
pNum = read <$> many ( oneOf "-0123456789")

pChar :: GenParser Char st Char
pChar = oneOf "|-LJ7F.S"

pFile :: GenParser Char st [String]
pFile = do
  lines <- many pChar `endBy` newline
  eof
  return lines

main = do
  block <- getContents
  print $ solve block
