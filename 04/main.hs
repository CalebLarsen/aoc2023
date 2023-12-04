import Text.ParserCombinators.Parsec
import qualified Data.Set as Set
import Data.Array

type Card = (Set.Set Int, [Int])

solve :: String -> Int
solve s =
  case finalCards of
    (Left _) -> 0
    (Right (Left _)) -> 0
    (Right (Right cs)) -> sum [cs!i | i <- [1..length cs]]
  where finalCards = (\x -> (\y -> addCardsN (length x) 1 y x) <$> vals) <$> arr
        arr   = (\x -> array (1,length x) [(i, 1) | i <- [1..length x]]) <$> vals
        vals  = map (`lineVal` 0) <$> cards
        cards = parse pCards "" s

addCard :: Int -> Int ->  Array Int Int -> Array Int Int
addCard _ 0 currCards = currCards
addCard i offset currCards = newCards // [(index, (newCards!index)+(newCards!i))]
  where newCards = addCard i (offset-1) currCards
        index = i+offset

addCardsN :: Int -> Int -> [Int] -> Array Int Int -> Array Int Int
addCardsN most i vals currCards
  | i > most = currCards
  | otherwise = addCardsN most (i+1) vals newCards
    where newCards = addCard i (vals!!(i-1)) currCards


lineVal :: Card -> Int -> Int
lineVal (x, y:ys) running
  | Set.member y x = if running == 0 then lineVal (x, ys) 1 else lineVal (x, ys) running+1
  | otherwise = lineVal (x, ys) running
lineVal (_, []) running = running

pNum :: GenParser Char st Int
pNum = read <$> many1 (oneOf "0123456789")

pCard :: GenParser Char st Card
pCard = do
  string "Card"
  many (char ' ')
  pNum
  string ":"
  many (char ' ')
  winningNums <- pNum `endBy` many (char ' ')
  string "|"
  many (char ' ')
  haveNums <- pNum `sepBy` many (char ' ')
  newline
  return (Set.fromList winningNums, haveNums)

pCards :: GenParser Char st [Card]
pCards = do
  cards <- many pCard
  --many (char '\n')
  eof
  return cards

main = do
  block <- getContents
  print $ solve block
