main :: IO()
main = do
    contents <- readFile "day02\\data_day02.txt"
    putStrLn (solve contents)

solve :: String -> String
solve inp = show (sum (map gamePossible (getFileLines inp)))

gamePossible :: String -> Integer
gamePossible game = if cubesPossible gameSets then game_id else 0
    where
        game_split = splitStrAt game ':' ""
        game_id = strToInt (last (splitStrAt (head game_split) ' ' ""))
        gameSets = map (\x -> splitStrAt x ',' "") (splitStrAt (last game_split) ';' "")

cubesPossible :: [[String]] -> Bool 
cubesPossible = all setPossible

setPossible :: [String] -> Bool
setPossible set = all (\x -> x) (map (\x -> let y = drop 1 (splitStrAt x ' ' "") in cubeCount (strToInt (head y)) (last y)) set)

cubeCount :: Integer -> String -> Bool
cubeCount count cube
    | cube == "blue" = count <= 14
    | cube == "green" = count <= 13
    | cube == "red" = count <= 12
    | otherwise = error("Unknown cube " ++ cube)

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
