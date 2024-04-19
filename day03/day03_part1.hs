import Data.Char (ord)

main :: IO()
main = do
    contents <- readFile "day03\\data_day03.txt"
    putStrLn (solve contents)

solve :: String -> String
solve inp = show (sum (adjacentNum (getFileLines inp) (0, 0) False ""))

adjacentNum :: [String] -> (Int, Int) -> Bool -> String -> [Integer]
adjacentNum flines pos@(x, y) adjacent num
    | length flines == y = [] 
    | length (flines !! y) == x = if adjacent && num /= "" 
        then strToInt num : adjacentNum flines (0, y+1) False ""
        else adjacentNum flines (0, y+1) False "" 
    | not (isDigit ((flines !! y) !! x)) = if adjacent && num /= ""
         then strToInt num : adjacentNum flines (x + 1, y) False ""
         else adjacentNum flines (x + 1, y) False ""
    | otherwise = adjacentNum flines (x + 1, y) (adjacent || isAdjacent flines pos) (num ++ [(flines !! y) !! x])

isAdjacent :: [String] -> (Int, Int) -> Bool
isAdjacent flines (x, y) = foldr 
    (\(a, b) acc -> if not acc && a >= 0 && b >= 0 && b < length flines && a < length (flines !! b) then ((flines !! b) !! a) /= '.' && not (isDigit ((flines !! b) !! a)) else acc)
    False
    [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1,y+1)]

isDigit :: Char -> Bool
isDigit c = ord c >= 48 && ord c <= 58



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
