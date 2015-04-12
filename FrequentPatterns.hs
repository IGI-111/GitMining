module FrequentPatterns (
    frequentPatterns
    ) where
import DataModel
import qualified Data.Set as Set
import Debug.Trace (trace)
import qualified Data.List as List
import Control.Parallel.Strategies(parMap, rpar)

semiUnion :: ItemSet -> ItemSet -> ItemSet
semiUnion (ItemSet set1) (ItemSet set2) = ItemSet $
    if max1 <= max2 && Set.delete max1 set1 == Set.delete max2 set2
    then set1 `Set.union` set2
    else Set.empty
        where
        max1 = Set.findMax set1
        max2 = Set.findMax set2

-- generate the next level in a bottom-up route
generateNextLevel :: [ItemSet] -> [ItemSet]
generateNextLevel level = trace ("Computing level " ++ show (isSize (head level))) $
    foldr (\value old -> generate value ++ old) [] level
        where
        generate value = takeWhile (/= empty) $
            parMap rpar (semiUnion value) (tail $ List.dropWhile (/= value) level) -- FIXME: this could be a better strategy
        isSize (ItemSet set) = Set.size set

singletons :: [ItemSet] -> [Item]
singletons table = Set.toList $ foldr union (Set.fromList []) table
    where
        union (ItemSet row) old = old `Set.union` row

frequentPatterns :: Frequency -> [ItemSet] -> [[ItemSet]]
frequentPatterns thresh table = until (\x -> [] == head x)
    (\x -> filterByFrequency (generateNextLevel (head x)) : x) [firstLevel]
        where
        firstLevel = filterByFrequency $ map (\x -> ItemSet $ Set.fromList [x]) $
            trace "Generated Singletons" (singletons table)
        filterByFrequency = filter (\x -> frequency table x >= thresh)
