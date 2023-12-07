import Text.ParserCombinators.Parsec
import Data.Either

solve :: String -> Int
solve s = unwrap
  where unwrap = fromRight 0 prod
        prod  = product <$> counts
        counts = map solveRace <$> races
        races  = parse pFile "" s

type Race = (Int, Int)

pNum :: GenParser Char st Int
pNum = read <$> many (oneOf "0123456789")

pFile :: GenParser Char st [Race]
pFile = do
  times <- pTimes
  let time = joinNums times
  distances <- pDistances
  let dist = joinNums distances
  return [(time, dist)]

pTimes :: GenParser Char st [Int]
pTimes = do
  string "Time:"
  many (char ' ')
  nums <- pNum `sepBy` many1 (char ' ')
  newline
  return nums

pDistances :: GenParser Char st [Int]
pDistances = do
  string "Distance:"
  many (char ' ')
  nums <- pNum `sepBy` many1 (char ' ')
  newline
  return nums


joinNums :: [Int] -> Int
joinNums xs = read $ concatMap show xs

getSpeeds :: Int -> [Int]
getSpeeds i = timeToSpeeds i 0

timeToSpeeds :: Int -> Int -> [Int]
timeToSpeeds 0 _ = []
timeToSpeeds time speed = time*speed:timeToSpeeds (time-1) (speed+1)

solveRace :: Race -> Int
solveRace (time, dist) = length $ filter (> dist) $ getSpeeds time

main = do
  block <- getContents
  print $ solve block
