import Data.Char (ord)
import Data.Set (fromList, toList)

removeDuplicates :: (Eq a, Ord a) => [a] -> [a]
removeDuplicates = toList . fromList

main :: IO()
main = do
    contents <- readFile "day03\\data_day03.txt"
    -- contents <- readFile "day03\\test.txt"
    putStrLn (solve contents)

solve :: String -> String
solve inp = show (calc (gearRatios (getFileLines inp) (0, 0)))

calc :: [[Integer]] -> Integer
calc = foldr (\x acc -> if null x then acc else acc + ((head x) * (x !! 1))) 0

gearRatios :: [String] -> (Int, Int) -> [[Integer]]
gearRatios flines (x, y)
    | length flines == y = []
    | length (flines !! y) == x = gearRatios flines (0, y+1)
    | (flines !! y) !! x == '*' = gearNums flines (x, y) : gearRatios flines (x + 1, y)
    | otherwise = gearRatios flines (x+1, y)


gearNums :: [String] -> (Int, Int) -> [Integer]
gearNums flines (x, y) =  res
    where 
        nums = foldr
            (\(a, b) acc -> if a >= 0 && b >= 0 && b < length flines && a < length (flines !! b) && isDigit ((flines !! b) !! a) then (a, b) : acc else acc)
            []
            [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1,y+1)]
        starts = removeDuplicates (map (findStart flines) nums)
        res = if length starts >= 2 && head starts /= starts !! 1 
            then [readNum flines (head starts) "", readNum flines (starts !! 1) ""]
            else []

readNum :: [String] -> (Int, Int) -> String -> Integer
readNum flines (x, y) num
    | length (flines !! y) == x || not (isDigit ((flines !! y) !! x)) = strToInt num
    | otherwise = readNum flines (x+1, y) (num ++ [(flines !! y) !! x])

findStart :: [String] -> (Int, Int) -> (Int, Int)
findStart flines (x, y)
    | x == 0 = (0, y)
    | not (isDigit ((flines !! y) !! (x-1))) = (x, y)
    | otherwise = findStart flines (x-1, y)

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
