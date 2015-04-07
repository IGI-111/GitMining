module ExtractRules (
    extractRules
) where
import DataModel
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map(Map)

extractRules :: Confidence -> [ItemSet] -> [ItemSet] -> [Rule]
extractRules threshold table patterns = filter (\x -> threshold <= confidence table x) rules where
    rules = Map.foldrWithKey (\k v old -> ruleFromSubset v k : old) [] subsets
    subsets = foldr (\x old -> insertMultiple (filteredPowerset x) x old) Map.empty patterns
    filteredPowerset (ItemSet set) = map (ItemSet . Set.fromList) $
        filter (\val -> val /= Set.toList set && val /= []) $ powerset $ Set.toList set

ruleFromSubset :: ItemSet -> ItemSet -> Rule
ruleFromSubset set subset = Rule subset (difference set subset)



insertMultiple :: Ord k => [k] -> a -> Map k a -> Map k a
insertMultiple keys value m = foldr (\x old -> Map.insert x value old) m keys

powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = xss /\/ map (x:) xss where
    xss = powerset xs

(/\/) :: [a] -> [a] -> [a]
[]     /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)

