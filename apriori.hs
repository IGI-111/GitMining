module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List

-- data structures defined here

data Item = Item String deriving (Eq, Ord)
instance Show Item where
	show (Item s) = s --"Item " ++ s

data ItemSet = ItemSet (Set Item) deriving (Eq, Ord)
instance Show ItemSet where
	show (ItemSet x) = let y = foldl (++) "" $ map show $ Set.toList x in y

type Frequency = Double

semiUnion :: ItemSet -> ItemSet -> ItemSet
semiUnion (ItemSet set1) (ItemSet set2) = ItemSet (if max1 <= max2 && Set.delete max1 set1 == Set.delete max2 set2 then set1 `Set.union` set2 else Set.empty) where
	max1 = Set.findMax set1
	max2 = Set.findMax set2

-- generate all possible combinations from a set of singletons
generateLevels :: [Item] -> [[ItemSet]]
generateLevels singles = until (\x -> head x == lastLevel) (\x -> generateNextLevel (head x) : x) [firstLevel] where
	firstLevel = map (\x -> ItemSet $ Set.fromList [x]) singles
	lastLevel = [ItemSet $ Set.fromList singles]

generateNextLevel :: [ItemSet] -> [ItemSet]
generateNextLevel level = foldr (\value old -> generate value ++ old) [] level where
	generate value = takeWhile (/= empty) (foldr (\x old -> semiUnion value x : old) [] (tail $ List.dropWhile (/= value) level)) where
		empty = ItemSet $ Set.fromList []

frequency :: [ItemSet] -> ItemSet -> Frequency
frequency table (ItemSet set) = setCount / fromIntegral (length table) where
	setCount = fromIntegral $ length (filter (\(ItemSet row) -> set `Set.isSubsetOf` row) table)

singletons :: [ItemSet] -> [Item]
singletons table = Set.toList $ foldr (\(ItemSet row) old -> old `Set.union` row) (Set.fromList []) table where


frequentPatterns :: Frequency -> [ItemSet] -> [[ItemSet]]
frequentPatterns thresh table = until (\x -> [] == head x) (\x -> filterByFrequency (generateNextLevel (head x)) : x) [firstLevel] where
	firstLevel = map (\x -> ItemSet $ Set.fromList [x]) (singletons table)
	filterByFrequency = filter (\x -> frequency table x >= thresh)


main :: IO()
main = print $ frequentPatterns 1 table where
	table = [ItemSet (Set.fromList [Item "a", Item "b", Item "c"]), ItemSet (Set.fromList [Item "a", Item "b"])]

