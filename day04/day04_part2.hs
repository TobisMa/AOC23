import Control.DeepSeq

main :: IO()
main = do
    contents <- readFile "day04\\data_day04.txt"
    -- contents <- readFile "day04\\test.txt"
    putStrLn (solve contents)

solve :: String -> String
solve inp = show (sum(inc(map winning files)))-- + length files)--files)
    where
        files = getFileLines inp


inc :: [Int] -> [Int]
inc cards = foldl 
    (\c i -> foldl 
        (\acc y -> addX acc y (c !! i))
        c
        [i + 1..min (i+cards !! i) (length c - 1)]
    ) 
    (ones (length cards)) 
    [0..length cards - 1]

ones :: Int -> [Int]
ones x = foldr (\_ acc -> 1 : acc) [] [1..x]

addX :: [Int] -> Int -> Int -> [Int]
addX lst i amount = if length lst <= i 
    then error("Upsi " ++ show i ++ show lst ++ show (length lst))
    else take i lst ++ [(lst !! i) + amount] ++ drop (i + 1) lst 

winning :: String -> Int
winning line = count win having
    where
        values = splitStrAt (last (splitStrAt line ':' "")) '|' ""
        win = map strToInt (filter (/= "") (splitStrAt (head values) ' ' ""))
        having = map strToInt (filter (/= "") (splitStrAt (last values) ' ' ""))
        num = count win having

count :: [Integer] -> [Integer] -> Int
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
