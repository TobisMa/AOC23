import Data.Char (ord)
main :: IO()
main = do
    contents <- readFile "day01\\data_day01.txt"
    putStrLn (solve contents)

solve :: String -> String
solve inp = show (sum (map twoDigit (getFileLines inp)))

twoDigit :: String -> Integer
twoDigit line = strToInt ([firstNumber line, lastNumber line])

firstNumber :: String -> Char
firstNumber = foldl (\acc x -> if (acc == '?') && (ord x <= 58) && (ord x >= 48) then x else acc) '?'

lastNumber :: String -> Char
lastNumber = foldr (\x acc -> if (acc == '?') && (ord x <= 58) && (ord x >= 48) then x else acc) '?'

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
