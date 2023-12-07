import Text.ParserCombinators.Parsec
import Data.Either

solve :: String -> Int
solve s = minimum locs
  where locs   = appMapp (maps!!6) hums
        hums   = appMapp (maps!!5) temps
        temps  = appMapp (maps!!4) lights
        lights = appMapp (maps!!3) waters
        waters = appMapp (maps!!2) ferts
        ferts  = appMapp (maps!!1) soils
        soils  = appMapp (head maps) seeds
        (seeds, maps) = fromRight ([], []) $ parse pFile "" s

pNum :: GenParser Char st Int
pNum = read <$> many1 (oneOf "0123456789")

pSeeds :: GenParser Char st [Int]
pSeeds = do
  string "seeds: "
  seeds <- pNum `sepBy` char ' '
  newline
  return seeds

data Rng = Rng {dst :: Int, src :: Int, len :: Int} deriving Show

pRng :: GenParser Char st Rng
pRng = do
  d <- pNum
  char ' '
  s <- pNum
  char ' '
  l <- pNum
  newline
  return $ Rng d s l

type Mapp = [Rng]

makeMap :: String -> GenParser Char st Mapp
makeMap s = do
  string s
  newline
  many pRng

pSeedSoilMap = makeMap "seed-to-soil map:"
pSoilFertMap = makeMap "soil-to-fertilizer map:"
pFertWaterMap = makeMap "fertilizer-to-water map:"
pWaterLightMap = makeMap "water-to-light map:"
pLightTempMap = makeMap "light-to-temperature map:"
pTempHumMap = makeMap "temperature-to-humidity map:"
pHumLocMap = makeMap "humidity-to-location map:"


pFile :: GenParser Char st ([Int], [Mapp])
pFile = do
  seeds <- pSeeds
  newline
  m0 <- pSeedSoilMap
  newline
  m1 <- pSoilFertMap
  newline
  m2 <- pFertWaterMap
  newline
  m3 <- pWaterLightMap
  newline
  m4 <- pLightTempMap
  newline
  m5 <- pTempHumMap
  newline
  m6 <- pHumLocMap
  eof
  return (seeds, [m0,m1,m2,m3,m4,m5,m6])

inRange :: Int -> Rng -> Bool
inRange i (Rng d s l)
  | i < s = False
  | i >= s+l = False
  | otherwise = True

srcToDst :: Int -> Rng -> Int
srcToDst i (Rng d s _) = (i - s) + d

mapOne :: Mapp -> Int -> Int
mapOne [] i = i
mapOne (x:xs) i = if i `inRange` x then srcToDst i x else mapOne xs i

appMapp :: Mapp -> [Int] -> [Int]
appMapp m = map (mapOne m)

main = do
  block <- getContents
  print $ solve block
