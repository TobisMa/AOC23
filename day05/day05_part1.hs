main :: IO()
main = do
    -- contents <- readFile "day05\\data_day05.txt"
    contents <- readFile "day05\\example.txt"
    putStrLn (solve contents)

solve :: String -> String
solve inp = show res--(foldr (\x acc -> min x acc) (head res) res)
    where
        parsed = parsing (getFileLines inp)
        seedNums = fst parsed
        maps = snd parsed
        res = map (`value` maps) seedNums

value :: Integer -> [CategoryConvert] -> Integer
value = foldl convertNum 

convertNum :: Integer -> CategoryConvert -> Integer
convertNum n (Category s d cs) = foldl (\acc x -> if inRange n x then conv n x else acc) n cs

inRange :: Integer -> Conversion -> Bool
inRange n (Convert s d l) = contains n (range s l) || contains n (range d l)

conv :: Integer -> Conversion -> Integer
conv n (Convert s d l) = s + n - d
    -- | contains n (range s l) = d + n - s
    -- | contains n (range d l) = s + n - d
    -- | otherwise = error "Ya messed up"

range :: Integer -> Integer -> [Integer]
range _ 0 = []
range start len = start : range (start + 1) (len - 1)

data Conversion = Convert {
    destRange :: Integer,
    sourceRange :: Integer,
    len :: Integer
}

data CategoryConvert = Category {
    source :: String,
    dest :: String,
    conversions :: [Conversion]
}

showCategoryConvert :: CategoryConvert -> String
showCategoryConvert (Category s d c) = "{source=" ++ s ++ ", target=" ++ d ++ ", destination=[" ++ concatStrs (map showConversion c) "," ++ "]}\n"

showConversion :: Conversion -> String
showConversion (Convert d s l) = "{destRange=" ++ show d ++ ", sourceRange=" ++ show s ++ ", len=" ++ show l ++ "}\n"


parsing :: [String] -> ([Integer], [CategoryConvert])
parsing (f:_:inp) = (seeds, maps)
    where
        seeds = map strToint (splitStrAt (drop 7 f) ' ' "")
        maps = getCategories inp

getCategories :: [String] -> [CategoryConvert]
getCategories [] = []
getCategories inp = if not (null (take l inp)) 
    then getCategory (take l inp) : getCategories (drop (l + 1) inp)
    else []
    where
        l = if null inp then 1 else find inp "" 0

getCategory :: [String] -> CategoryConvert
getCategory [] = error "Empty0"
getCategory [""] = error "Empty1"
getCategory (f:o) = Category s t convs
    where
        types = splitStrAt (head (splitStrAt f ' ' "")) '-' ""
        s = head types
        t = last types
        convs = map (\x -> getConverts (splitStrAt x ' ' "")) (filter (/="") o)

getConverts :: [String] -> Conversion
getConverts [a, b, c] = Convert (strToint a) (strToint b) (strToint c)

find :: [String] -> [Char] -> Int -> Int
find [] f i = i
find l f i
    | head l == f = i
    | otherwise = find (tail l) f (i+1)

contains :: Eq a => a -> [a] -> Bool
contains v = foldr (\x acc -> if not acc then x == v else acc) False

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
