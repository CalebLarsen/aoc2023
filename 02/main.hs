import Data.List.Split

splitOnn del s = filter (not . null) $ splitOn del s
splitChunk = splitOnn "\n\n"
splitLines = splitOnn "\n"
solve :: String -> Int
solve = processBlock

stripPrefix :: Char -> String -> String
stripPrefix c (x:xs) = if x == c then xs else stripPrefix c xs
stripPrefix _ "" = ""

stripSuffix :: Char -> String -> String
stripSuffix c s = reverse $ stripPrefix c $ reverse s

getGameID :: String -> Int
getGameID s = read $ stripPrefix ' ' $ stripSuffix ':' s

getGames :: String -> String
getGames = stripPrefix ':'

getList :: String -> [String]
getList = splitOnn ";"

data Color = Red | Green | Blue deriving Show
data Pull =
  Pull
  {
    c :: Maybe Color
  , count :: Int
  } deriving Show

processColor :: String -> Maybe Color
processColor "red" = Just Red
processColor "green" = Just Green
processColor "blue" = Just Blue
processColor _ = Nothing

first (x:xs) = x
first _ = []

end [x] = x
end (x:xs) = end xs
end _ = []
processPull :: String -> Pull
processPull s = Pull (processColor $ end (splitOnn " " s)) (read $ first (splitOnn " " s))

processPulls :: String -> [Pull]
processPulls s = map processPull $ splitOnn "," s

data Power =
  Power
  {
    red :: Int
  , green :: Int
  , blue :: Int
  } deriving Show

emptyPower = Power 0 0 0

pullToPower :: Pull -> Power
pullToPower (Pull (Just Red) count) = Power count 0 0
pullToPower (Pull (Just Green) count) = Power 0 count 0
pullToPower (Pull (Just Blue) count) = Power 0 0 count
pullToPower _ = emptyPower

andPower :: Power -> Power -> Power
andPower (Power r1 g1 b1) (Power r2 g2 b2) = Power (max r1 r2) (max g1 g2) (max b1 b2)

powerVal :: Power -> Int
powerVal (Power r g b) = r * g * b

pullsToPower :: [Pull] -> Power
pullsToPower = foldr (andPower . pullToPower) emptyPower

processGame :: String -> Int
processGame s = powerVal $ foldr (andPower . pullsToPower . processPulls) emptyPower (getList s)
{--
validPull :: Pull -> Bool
validPull (Pull Nothing _) = False
validPull (Pull (Just Red) count) = count <= 12
validPull (Pull (Just Green) count) = count <= 13
validPull (Pull (Just Blue) count) = count <= 14

validPulls :: [Pull] -> Bool
validPulls = all validPull

processGame :: String -> Bool
processGame s = all (validPulls . processPulls) (getList s)
--}
toSum :: Int -> Bool -> Int
toSum _ False = 0
toSum c True = c

parts :: String -> (Int, String)
parts s = (getGameID s, getGames s)

{--
processLine :: String -> Int
processLine s = toSum (fst (parts s)) (processGame (snd (parts s)))
--}
processLine :: String -> Int
processLine s = processGame $ snd $ parts s
processBlock :: String -> Int
processBlock s = sum $ map processLine (splitLines s)

main = do
  block <- getContents
  print $ solve block
