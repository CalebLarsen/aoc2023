import Text.ParserCombinators.Parsec
import qualified Data.Set as Set


type Card = (Set.Set Int, [Int])

solve :: String -> Int
solve s =
  case val of
    (Left p) -> 0
    (Right v) -> v
  where val = sum . map (`lineVal` 0) <$> cards
        cards = parse pCards "" s

lineVal :: Card -> Int -> Int
lineVal (x, y:ys) running
  | Set.member y x = if running == 0 then lineVal (x, ys) 1 else lineVal (x, ys) 2*running
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
