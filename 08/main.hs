import Text.ParserCombinators.Parsec
import Data.Map qualified as M
import Data.Either qualified as E
import Data.Maybe (fromJust)

solve :: String -> Integer
solve s = foldr lcm 1 stepsL 
  where steps  = mazeStep m inst "AAA"
        stepsL = map (mazeStep m inst) starts
        starts = M.keys $ M.filterWithKey (\k _ -> k!!2 == 'A') m
        m      = M.fromList $ snd p
        inst   = cycle $ fst p
        p      = E.fromRight ("",[("",("", ""))]) parsed
        parsed = parse pFile "" s

mazeStep :: M.Map String (String, String) -> String -> String -> Integer
mazeStep _ [] _ = 987654321
mazeStep m (x:inst) curr
  | curr!!2 == 'Z' = 0
  | otherwise = 1 + mazeStep m inst newCurr
  where newCurr = which x (fromJust $ M.lookup curr m)

which :: Char -> (String, String) -> String
which c (l, r)
  | c == 'L' = l
  | c == 'R' = r
  | otherwise = "WHICH WAS WRONG"

pLabel :: GenParser Char st String
pLabel = many $ oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

pLine :: GenParser Char st (String, (String, String))
pLine = do
  a <- pLabel
  string " = ("
  l <- pLabel
  string ", "
  r <- pLabel
  char ')'
  newline
  return (a, (l, r))

pFile = do
  inst <- many (oneOf "LR")
  newline
  newline
  info <- many pLine
  eof
  return (inst, info)

main = do
  block <- getContents
  print $ solve block
