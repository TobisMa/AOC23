main :: IO()
main = do
    -- contents <- readFile "day06\\data_day06.txt"
    contents <- readFile "day06\\example.txt"
    putStrLn (solve contents)

solve :: String -> String
solve inp = show (calc (races (getFileLines inp)))

calc :: (Float, Float) -> Integer
calc (time, distance) = (floor (e1 + 0.5)) - (floor (e2 + 0.5)) - 1 --off by 1..4  (depends on the interval; don't wan to add an iterative solution)
    where
        (e1, e2) = f time distance

f time d = (time / 2 + sqrt ((time / 2) ** 2 - d), time / 2 - sqrt ((time / 2) ** 2 - d))

untilWin2 :: (Integer, Integer) -> Integer -> Integer
untilWin2 (time, distance) x
    | (time - x) * x <= distance = untilWin2 (time, distance) x
    | otherwise = x

untilWin :: (Integer, Integer) -> Integer -> Integer
untilWin (time, distance) x 
    | (time - x) * x <= distance = untilWin (time, distance) x
    | otherwise = x


races :: [String] -> (Float, Float)
races (first:second:_) = (time, distance)
    where
        time = strToFloat (concatStrs (filter (/="") (splitStrAt (drop 11 first) ' ' "")) "")
        distance = strToFloat (concatStrs (filter (/="") (splitStrAt (drop 11 second) ' ' "")) "")




getFileLines :: String -> [String] 
getFileLines = lines

concatValuesByLine :: Show a => [a] -> String
concatValuesByLine v = unlines (map show v)

concatStrsByLine :: [String] -> String
concatStrsByLine = unlines

concatValues :: Show a => [a] -> String
concatValues = concatMap show

concatStrs :: [String] -> String -> String 
concatStrs (first:strs) sep = foldl (\acc x -> acc ++ sep ++ x) first strs

valuesToStr :: Show a => [a] -> [String]
valuesToStr = map show

splitStrAt :: String -> Char -> String -> [String]
splitStrAt "" _ buf = [buf | buf /= ""]
splitStrAt (c:s) del buf
    | c == del = buf : splitStrAt s del ""
    | otherwise = splitStrAt s del (buf ++ [c])

strToint :: String -> Integer
strToint s = read s :: Integer

strToFloat :: String -> Float
strToFloat s = read s :: Float

strToDouble :: String -> Double
strToDouble s = read s :: Double
