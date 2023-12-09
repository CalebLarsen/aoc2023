import Data.List
import Text.ParserCombinators.Parsec
import Data.Either

solve :: String -> Int
solve s = unwrap
  where unwrap = fromRight 0 bets2
        bets2  = sum <$> bets1
        bets1  = map (\((_, x), y) -> x*y) <$> bets0
        bets0  = (\x -> zip x [1..])  <$> sHands
        sHands = sortBy (\(x,_) (y,_) -> sortHands x y) <$> ins
        ins = parse pFile "" s

mostLetter :: String -> Char
mostLetter s = fst $ last dumb
  where dumb     = if null noJoke then [('A', 1)] else noJoke
        noJoke   = filter (\(x, _) -> x /= 'J') sLetters
        sLetters = sortBy sortLetters letters
        letters  = countLetters s

sortLetters :: (Char, Int) -> (Char, Int) -> Ordering
sortLetters (c0, v0) (c1, v1)
  | v0 /= v1 = compare v0 v1
  | otherwise = compare (rankCard c0) (rankCard c1)

countLetters :: String -> [(Char, Int)]
countLetters s = zip s $ map (countLetter s) s

countLetter :: String -> Char -> Int
countLetter [] _ = 0
countLetter (x:xs) c
  | x == c = 1 + countLetter xs c
  | otherwise = countLetter xs c

pNum :: GenParser Char st Int
pNum = read <$> many (oneOf "0123456789")

pHand :: GenParser Char st (String, Int)
pHand = do
  hand <- many (oneOf "23456789TJQKA")
  char ' '
  bet <- pNum
  many (char ' ')
  newline
  return (hand, bet)

pFile :: GenParser Char st [(String, Int)]
pFile = many pHand

countt :: String -> Char -> Int
countt [] _ = 0
countt (x:xs) c
  | x == c = 1 + countt xs c
  | otherwise = countt xs c

countF :: String -> Int
countF s = countt s (head s)

isFive :: String -> Int
isFive s
  | countF s == 5 = 7
  | otherwise = 0

isFour :: String -> Int
isFour [] = 0
isFour (x:xs)
  | countF (x:xs)  == 4 = 6
  | countF xs  == 4 = 6
  | otherwise = 0

isFullHouse :: String -> Int
isFullHouse [] = 0
isFullHouse s
  | length (nub s) == 2 = 5 -- Triggers on 4 of a kind
  | otherwise = 0

isThree [] = 0
isThree [x] = 0
isThree (x:y:xs)
  | countF (x:y:xs) == 3 = 4
  | countF (y:xs) == 3 = 4
  | countF xs == 3 = 4
  | otherwise = 0

isTwoPair [] = 0
isTwoPair s
  | length (nub s) == 3 = 3
  | otherwise = 0

isPair :: String -> Int
isPair [] = 0
isPair s
  | length (nub s) == 4 = 2
  | otherwise = 0

isHigh :: String -> Int
isHigh s = 1

rankCard :: Char -> Int
rankCard c
  | c == 'A' = 14
  | c == 'K' = 13
  | c == 'Q' = 12
  | c == 'J' = 1
  | c == 'T' = 10
  | c == '9' = 9
  | c == '8' = 8
  | c == '7' = 7
  | c == '6' = 6
  | c == '5' = 5
  | c == '4' = 4
  | c == '3' = 3
  | c == '2' = 2
  | otherwise = 0

compCards :: Char -> Char -> Ordering
compCards x y = compare (rankCard x) (rankCard y)

solveTie :: String -> String -> Ordering
solveTie [] [] = EQ
solveTie [] _  = LT
solveTie  _ [] = GT
solveTie (x:xs) (y:ys)
  | compCards x y /= EQ = compCards x y
  | otherwise = solveTie xs ys

handVal :: String -> Int
handVal s = maximum [isFive s, isFour s, isFullHouse s, isThree s, isTwoPair s, isPair s, isHigh s]

sortHands :: String -> String -> Ordering
sortHands x y
  | handVal (unJoke x) < handVal (unJoke y) = LT
  | handVal (unJoke x) > handVal (unJoke y) = GT
  | otherwise = solveTie x y

replace :: String -> Char -> Char -> String
replace [] _ _ = []
replace (x:xs) old new
  | x == old = new:replace xs old new
  | otherwise = x:replace xs old new

unJoke :: String -> String
unJoke s = replace s 'J' most
  where most = mostLetter s

showHands :: [(String, Int)] -> String
showHands xs = concatMap (\x -> fst (xs!!x) ++ "\n") [0..length xs - 1]

getInfo :: String -> String
getInfo s = fromRight [] sh
  where sh = showHands <$> sHands
        sHands = sortBy (\(x,_) (y,_) -> sortHands x y) <$> ins
        ins = parse pFile "" s

main = do
  block <- getContents
  print $ solve block
--  putStr $ getInfo block
