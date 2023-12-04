main :: IO()
main = do
    contents <- readFile "day04\\data_day04.txt"
    putStrLn (solve contents)

solve :: String -> String
solve inp = show (sum (map winning (getFileLines inp)))

-- winning :: String -> Integer
winning line = if count win having == 0 then 0 else 2 ^ count win having
    where
        values = splitStrAt (last (splitStrAt line ':' "")) '|' ""
        win = map strToInt (filter (/= "") (splitStrAt (head values) ' ' ""))
        having = map strToInt (filter (/= "") (splitStrAt (last values) ' ' ""))
        num = count win having

count :: [Integer] -> [Integer] -> Integer
count a b = foldr (\x acc -> if contains x b then acc + 1 else acc) 0 a

contains :: Eq a => a -> [a] -> Bool
contains value = foldr (\x acc -> if not acc then x == value else acc) False


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

strToInt :: String -> Integer
strToInt s = read s :: Integer

strToFloat :: String -> Float
strToFloat s = read s :: Float

strToDouble :: String -> Double
strToDouble s = read s :: Double
