{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
import Text.ParserCombinators.Parsec

data Color = Red | Green | Blue deriving Show

data Draw = Draw
 { count :: Int
 , color :: Color
 } deriving Show

data Pull = Pull {
  draw :: [Draw]
} deriving Show

data Game = Game
 { id :: Int
 , pulls :: [Pull]
 } deriving Show

pNum :: GenParser Char st Int
pNum = read <$> many (oneOf "0123456789")

pFile :: GenParser Char st [Game]
pFile = pGame `endBy` newline <* eof

pGame :: GenParser Char st Game
pGame = do
  string "Game "
  id <- pNum
  string ": "
  pulls <- pPull `sepBy` string "; "
  return $ Game id pulls

pPull :: GenParser Char st Pull
pPull = do
  draws <- pDraw `sepBy` string ", "
  return $ Pull draws

pDraw :: GenParser Char st Draw
pDraw = do
  count <- pNum
  char ' '
  Draw count <$> pColor

pColor :: GenParser Char st Color
pColor = chooseColor <$> (string "red" <|> string "green" <|> string "blue")
  where chooseColor "red" = Red
        chooseColor "green" = Green
        chooseColor "blue" = Blue

validGame :: Game -> Int
validGame (Game id ps) = if all validPull ps then id else 0

validPull :: Pull -> Bool
validPull (Pull ds) = all validDraw ds

validDraw :: Draw -> Bool
validDraw (Draw num Red)   = num <= 12
validDraw (Draw num Green) = num <= 13
validDraw (Draw num Blue)  = num <= 14

{--
GRAMMAR 
S     -> (Game "\n")+ eof
Game  -> "Game " num ": " Pull ("; " Pull)*
Pull  -> Draw (", " Draw)* 
Draw  -> num " " color
color -> "red" | "green" | "blue"
num   -> [0-9]+
--}

data Power = Power
  { red   :: Int
  , green :: Int
  , blue  :: Int
  }
zeroPower = Power 0 0 0
andPower (Power a b c) (Power d e f) = Power (max a d) (max b e) (max c f)
andDraw (Draw n Red) (Power a b c) = Power (max a n) b c
andDraw (Draw n Green) (Power a b c) = Power a (max b n) c
andDraw (Draw n Blue) (Power a b c) = Power a b (max c n)

pullPower (Pull ds) = foldr andDraw zeroPower ds
gamePower (Game _ ps) = foldr (andPower . pullPower) zeroPower ps
powerInt (Power a b c) = a * b * c
{--
main = do
  block <- getContents
  print (sum . map validGame <$> parse pFile "" block)
--}
main = do
  block <- getContents
  print $ sum . map (powerInt . gamePower) <$> parse pFile "" block
