import Prelude hiding (splitAt)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Char (ord, chr)


main :: IO()
main = do
    contents <- readFile "day15/data_day15.txt"
    putStrLn (solve contents)

solve :: String -> String
-- solve inp = show inp
solve inp = show(sum(map hashing (splitStr (removeLast inp) "," "")))

hashing :: String -> Int
hashing s = foldl singleHash 0 s

singleHash :: Int -> Char -> Int
singleHash curVal c = ((ord c + curVal) * 17) `mod` 256

removeLast :: String -> String
removeLast s = take (length s - 1) s

-- matrix
type CharGrid = Matrix Char
type Matrix a = [Row a]
type Row a = [a]

matToList :: Matrix a -> [[a]]
matToList m = m

matGet :: Matrix a -> Int -> Int -> a
matGet m x y = (m !! y) !! x

matSet :: Matrix a -> Int -> Int -> a -> Matrix a
matSet m x y value = take y m ++ [rowSet (m !! y) x value] ++ drop (y+1) m

matMap :: (a -> b) -> Matrix a -> Matrix b
matMap f = map (map f)

matRowMap :: (Row a -> Row b) -> Matrix a -> Matrix b
matRowMap = map

rowSet :: Row a -> Int -> a -> Row a
rowSet r i v = take i r ++ [v] ++ drop (i+1) r

charGridFromString :: String -> String -> CharGrid
charGridFromString str lineDel = splitStr str lineDel ""

matEmpty :: Int -> Int -> a -> Matrix a
matEmpty rowCount lineCount fill = replicate lineCount (replicate rowCount fill)

gridEmpty :: Int -> Int -> Char -> CharGrid
gridEmpty = matEmpty

matFromString :: String -> String -> String -> Matrix String
matFromString str lineDel rowDel = map (`rowFromString` rowDel) (splitStr str lineDel "")

matFromList :: [[a]] -> Matrix a
matFromList lst = map rowFromList lst

rowFromString :: String -> String -> Row String
rowFromString str del = splitStr str del ""

rowFromList :: [a] -> Row a
rowFromList lst = lst

showGrid :: CharGrid -> String
showGrid g = concatStrs g "\n" ++ "\n"

-- sets
setFromList :: Ord a => [a] -> Set a
setFromList = Set.fromList

setToList :: Set a -> [a]
setToList = Set.toList

setInit :: Ord a => Set a
setInit = setFromList []

-- maps
mapInit :: Ord k => Map k v
mapInit = mapFromList []

mapFromList :: Ord k => [(k, v)] -> Map k v
mapFromList lst = Map.fromList lst

mapToList :: Ord k => Map k v -> [(k, v)]
mapToList m = Map.toList m



-- other
removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates lst = setToList (setFromList lst)

removeDuplicatesRec :: Eq a => [a] -> [a]
removeDuplicatesRec [] = []
removeDuplicatesRec (x:xs) = x : removeDuplicatesRec (filter (/= x) xs)


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

-- keep splitStrAt because of already existing solutions. Remove for next year
splitStrAt :: String -> Char -> String -> [String]
splitStrAt "" _ buf = [buf | buf /= ""]
splitStrAt (c:s) del buf
    | c == del = buf : splitStrAt s del ""
    | otherwise = splitStrAt s del (buf ++ [c])

splitStr :: String -> String -> String -> [String]
splitStr "" _ buf = [buf | buf /= ""]   -- i am aware of the mistake that "bababa" splitted by a [b, b, b] returns; not what is expected: [b, b, b, ""] -> that mistake works wonderfully when parsing mistakes 
splitStr s del buf
    | next == del = buf : splitStr following del ""
    | otherwise = splitStr (tail s) del (buf ++ [head s])
    where
        next = take (length del) s
        following = drop (length del) s

strToInteger :: String -> Integer
strToInteger s = read s :: Integer

strToInt :: String -> Int
strToInt s = read s :: Int

strToFloat :: String -> Float
strToFloat s = read s :: Float

strToDouble :: String -> Double
strToDouble s = read s :: Double

chrToDigit :: Char -> Int
chrToDigit c = if o >= 48 && o <= 58 then o - 48 else error (c : " is not a valid digit")
    where
        o = ord c

chrToLargeDigit :: Char -> Integer
chrToLargeDigit '0' = 0
chrToLargeDigit '1' = 1
chrToLargeDigit '2' = 2
chrToLargeDigit '3' = 3
chrToLargeDigit '4' = 4
chrToLargeDigit '5' = 5
chrToLargeDigit '6' = 6
chrToLargeDigit '7' = 7
chrToLargeDigit '8' = 8
chrToLargeDigit '9' = 9
chrToLargeDigit s = error (s : " is not a valid digit")

countOccurences :: Ord a => [a] -> Map a Int
countOccurences = foldr (\x acc -> if Map.member x acc then Map.insert x ((acc ! x) + 1) acc else Map.insert x 1 acc) Map.empty

sortBy :: Ord a => (a -> a -> Bool) -> [a] -> [a]
sortBy _ [] = []
sortBy f (x:lst) = left ++ [x] ++ right
    where
        left = sortBy f (filter (f x) lst)
        right = sortBy f (filter (not . f x) lst)

isDigit :: Char -> Bool
isDigit c = ord c >= 48 && ord c <= 58

isNumber :: String -> Bool
isNumber = all isDigit

contains :: Eq a => a -> [a] -> Bool
contains v = foldr (\x acc -> if not acc then x == v else acc) False
