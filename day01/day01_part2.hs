import Data.Char (ord)
main :: IO()
main = do
    contents <- readFile "day01\\data_day01.txt"
    putStrLn (solve contents)

solve :: String -> String
solve inp = show (sum (map twoDigit (getFileLines inp)))

twoDigit :: String -> Integer
twoDigit line = firstNumber line * 10 + lastNumber line

firstNumber :: String -> Integer
firstNumber line = foldl (\acc x -> if acc == -1 && isDigit (drop x line) /= -1 then isDigit (drop x line) else acc) (-1) [0..length line]

lastNumber :: String -> Integer
lastNumber line = foldl (\acc x -> if acc == (-1) then isDigit (drop x line) else acc) (-1) (reverse [0..length line])

isDigit :: String -> Integer
isDigit digit
    | null digit = -1
    | ord (head digit) >= 48 && ord (head digit) <= 58 = strToInt [head digit]
    | take 3 digit == "one" = 1
    | take 3 digit == "two" = 2
    | take 5 digit == "three" = 3
    | take 4 digit == "four" = 4
    | take 4 digit == "five" = 5
    | take 3 digit == "six" = 6
    | take 5 digit == "seven" = 7
    | take 5 digit == "eight" = 8
    | take 4 digit == "nine" = 9
    -- | take 4 digit == "zero" = 0
    | otherwise = -1


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
