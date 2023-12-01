import Data.List.Split
import Data.List
import Data.Char

splitChunk = splitOn "\n\n"
splitLines s = filter (not . null) $ splitOn "\n" s
solve :: String -> Int
solve s = sum $ map (processLine . pp) (splitLines s)

firstD :: String -> Maybe Int
-- firstD s = s >>= (\x -> if isDigit x then Just (ord x - ord '0') else Nothing)
firstD [] = Nothing
firstD (x:xs) = if isDigit x then Just (ord x - ord '0') else firstD xs

lastD :: String -> Maybe Int
lastD s = firstD $ reverse s

getNum :: String -> Maybe Int
getNum s = firstD s >>= (\x -> lastD s >>= (\y -> Just $ (x*10) + y))

firmUp :: Maybe Int -> Int
firmUp (Just x) = x
firmUp _ = 0

processLine :: String -> Int
processLine s = firmUp $ getNum s

ppLine :: String -> String -> String
ppLine _ [] = []
ppLine "one"  (x:xs) = if "one" `isPrefixOf` (x:xs) then x:'1':x:ppLine "one" xs else x:ppLine "one" xs 
ppLine "two"  (x:xs) = if "two" `isPrefixOf` (x:xs) then x:'2':x:ppLine "two" xs else x:ppLine "two" xs 
ppLine "three"  (x:xs) = if "three" `isPrefixOf` (x:xs) then x:'3':x:ppLine "three" xs else x:ppLine "three" xs 
ppLine "four"  (x:xs) = if "four" `isPrefixOf` (x:xs) then x:'4':x:ppLine "four" xs else x:ppLine "four" xs 
ppLine "five"  (x:xs) = if "five" `isPrefixOf` (x:xs) then x:'5':x:ppLine "five" xs else x:ppLine "five" xs 
ppLine "six"  (x:xs) = if "six" `isPrefixOf` (x:xs) then x:'6':x:ppLine "six" xs else x:ppLine "six" xs 
ppLine "seven"  (x:xs) = if "seven" `isPrefixOf` (x:xs) then x:'7':x:ppLine "seven" xs else x:ppLine "seven" xs 
ppLine "eight"  (x:xs) = if "eight" `isPrefixOf` (x:xs) then x:'8':x:ppLine "eight" xs else x:ppLine "eight" xs 
ppLine "nine"  (x:xs) = if "nine" `isPrefixOf` (x:xs) then x:'9':x:ppLine "nine" xs else x:ppLine "nine" xs 
ppLine "zero"  (x:xs) = if "zero" `isPrefixOf` (x:xs) then x:'0':x:ppLine "zero" xs else x:ppLine "zero" xs 
ppLine _ _ = []

pp :: String -> String
pp s = ppLine "one" $
       ppLine "two" $
       ppLine "three" $
       ppLine "four" $
       ppLine "five" $
       ppLine "six" $
       ppLine "seven" $
       ppLine "eight" $
       ppLine "nine" $
       ppLine "zero" s

main = do
  block <- getContents
  print $ solve block
