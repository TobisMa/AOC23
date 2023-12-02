main :: IO()
main = do
    contents <- readFile "day02\\data_day02.txt"
    putStrLn (solve contents)

solve :: String -> String
solve inp = show (sum (map gamePossible (getFileLines inp)))

power :: (Integer, Integer, Integer) -> Integer
power (r, g, b) = r * g * b

gamePossible :: String -> Integer
gamePossible game = power (foldr (setcmp . setPower) (0, 0, 0) gameSets)
    where
        game_split = splitStrAt game ':' ""
        gameSets = map (\x -> splitStrAt x ',' "") (splitStrAt (last game_split) ';' "")

setcmp :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
setcmp (a, b, c) (r, g, bl) = (max a r, max b g, max c bl)

setPower :: [String] -> (Integer, Integer, Integer)
setPower = foldr (\cube acc -> let y = drop 1 (splitStrAt cube ' ' "") in check y acc) (0, 0, 0)

check :: [String] -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
check [count, color] (r, g, b) 
    | color == "red" = (max r (strToInt count), g, b)
    | color == "green" = (r, max g (strToInt count), b)
    | color == "blue" = (r, g, max b (strToInt count))
    | otherwise = error ("what " ++ count ++ "  " ++ color ++ "  " ++ show (r, g, b))
check wierd _ = error (show wierd)



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
