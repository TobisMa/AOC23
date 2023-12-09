import Prelude hiding (splitAt)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List (findIndex, elemIndex, maximumBy)
import qualified Data.List as List
import Data.Function (on)


main :: IO()
main = do
    contents <- readFile "day08\\data_day08.txt"
    -- contents <- readFile "day08\\flo8.txt"
    -- contents <- readFile "day08\\example.txt"
    putStrLn (solve contents)

solve :: String -> String
solve inp = show (res)
    where
        (directions, other) = parse inp
        mL = matToList other
        pmap = mapFromList (map convertInp mL)
        starts = filter (\x -> (x !! 2) == 'A') (Map.keys pmap)
        values = map (countWalkToFirstZ pmap (cycle directions)) starts
        res = foldr lcm 1 values
        

countWalkToFirstZ :: Map String (String, String) -> String -> String -> Integer
countWalkToFirstZ _ [] _ = error "No directions left"
countWalkToFirstZ walk (d:directions) pos
    | pos !! 2 == 'Z' = 0
    | d == 'L' = 1 + countWalkToFirstZ walk directions (fst (walk ! pos))
    | d == 'R' = 1 + countWalkToFirstZ walk directions (snd (walk ! pos))
    | otherwise = error ("Wow: pos=" ++ pos ++ "; d=" ++ [d])

convertInp :: [String] -> (String, (String, String))
convertInp [s, l] = (s, (head out, take 3 (last out)))
    where
        out = splitStr l ", " ""
convertInp a = error ("Upsi: " ++ show a)

parse :: String -> (String, Matrix String)
parse inp = (start, rest)
    where
        splitted =splitStr inp "\n\n" ""
        start = head splitted
        rest = matFromString (last splitted) "\n" " = ("

-- matrix
type CharGrid = Matrix Char
type Matrix a = [Row a]
type Row a = [a]

matGet :: Matrix a -> Int -> Int -> a
matGet m x y = (m !! y) !! x

matToList :: Matrix a -> [[a]]
matToList m = m

matSet :: Matrix a -> Int -> Int -> a -> Matrix a
matSet m x y value = take y m ++ [rowSet (m !! y) x value] ++ drop (y+1) m

matMap :: (a -> b) -> Matrix a -> Matrix b
matMap f m = map (\x -> map f x) m

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
splitStr "" _ buf = [buf | buf /= ""]
splitStr s del buf
    | next == del = buf : splitStr following del ""
    | otherwise = splitStr (tail s) del (buf ++ [head s])
    where
        next = take (length del) s
        following = drop (length del) s

strToint :: String -> Integer
strToint s = read s :: Integer

strToFloat :: String -> Float
strToFloat s = read s :: Float

strToDouble :: String -> Double
strToDouble s = read s :: Double
