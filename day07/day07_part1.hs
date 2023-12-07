import Prelude hiding (splitAt)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Char (ord, chr)
import Data.List (sort, sortOn, groupBy)


main :: IO()
main = do
    contents <- readFile "day07\\data_day07.txt"
    -- contents <- readFile "day07\\example.txt"
    putStrLn (solve contents)

solve :: String -> String
solve inp = show (sum (foldl (\acc x -> bidding mHands x acc) [] sorted))
    where
        hands = map (\[a, b] -> (a, strToint b)) (matFromString inp "\n" " ")
        mHands = mapFromList hands
        sortHands = sortOn (\(x, _) -> getType (count x)) hands
        cs = groupBy (\(a, b) (c, d) -> getType (count a) == getType (count c)) sortHands
        sorted = flatten (map (sortBy (\(a, _) (b, _) -> differ a b)) cs)

bidding :: Map String Int -> (String, Int) -> [Int] -> [Int]
bidding hands hand acc = ((hands ! fst hand) * (length acc + 1)) : acc

len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

getType :: Map Char Int -> Int
getType = rankCase . sort . Map.elems

flatten :: [[a]] -> [a]
flatten = concat

cards :: Map Char Int
cards = mapFromList [('A', 14), ('K', 13), ('Q', 12), ('J', 11), ('T', 10), ('9', 9), ('8', 8), ('7', 7), ('6', 6), ('5', 5), ('4', 4), ('3', 3), ('2', 2), ('1', 1)]

highestCard :: String -> Int
highestCard hand = maximum (map (cards !) hand)

differ :: String -> String -> Bool
differ [] [] = False
differ (a:as) (b:bs)
    | a == b = differ as bs
    | otherwise = cards ! a > cards ! b

sortBy :: Ord a => (a -> a -> Bool) -> [a] -> [a]
sortBy _ [] = []
sortBy f (x:lst) = left ++ [x] ++ right
    where
        left = sortBy f (filter (f x) lst)
        right = sortBy f (filter (not . f x) lst)

count :: String -> Map Char Int
count = foldr (\x acc -> if Map.member x acc then Map.insert x ((acc ! x) + 1) acc else Map.insert x 1 acc) Map.empty
-- cards :: Matrix String -> 

rankCase :: [Int] -> Int
rankCase [5] = 7        -- five pair
rankCase [1, 4] = 6     -- four pair
rankCase [2, 3] = 5     -- full house
rankCase [1,1,3] = 4    -- three of a kind
rankCase [1, 2, 2] = 3  -- two pair
rankCase [1, 1, 1, 2] = 2  -- one pair
rankCase [1, 1, 1, 1, 1] = 1  -- high card
rankCase l = error ("For " ++ show l ++ "! Wtf")

-- matrix
type CharGrid = Matrix Char
type Matrix a = [Row a]
type Row a = [a]

matGet :: Matrix a -> Int -> Int -> a
matGet m x y = (m !! y) !! x

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

-- matFromList :: [a] -> Matrix a
-- matFromList lst = map rowFromList lst

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

strToint :: String -> Int
strToint s = read s :: Int

strToFloat :: String -> Float
strToFloat s = read s :: Float

strToDouble :: String -> Double
strToDouble s = read s :: Double
