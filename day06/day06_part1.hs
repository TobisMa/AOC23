main :: IO()
main = do
    contents <- readFile "day06\\data_day06.txt"
    -- contents <- readFile "day06//example.txt"
    putStrLn (solve contents)

solve :: String -> String
solve inp = show (product(calc (races (getFileLines inp))))

calc :: [(Integer, Integer)] -> [Integer]
calc races = map 
    (\(time, distance) -> foldr 
        (\x acc -> if (time - x) * x > distance then acc + 1 else acc)
        0
        [1..time]
    )
    races

races :: [String] -> [(Integer, Integer)]
races (first:second:_) = zip time distance
    where
        time = map strToint (filter (/="") (splitStrAt (drop 11 first) ' ' ""))
        distance = map strToint (filter (/="") (splitStrAt (drop 11 second) ' ' ""))




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
