import Text.ParserCombinators.Parsec
import Data.Either

solve :: String -> Int
solve s = read s
  where f = s

pNum :: GenParser Char st Int
pNum = read <$> many ( oneOf "-0123456789")

main = do
  block <- getContents
  print $ solve block
