module DataModel where
import Data.Set (Set)
import qualified Data.Set as Set

type Count = Int

type Frequency = Double
type Confidence = Double
type Lift = Double

class Freq a where
    frequency :: [ItemSet] -> a -> Frequency

data Item = Item String deriving (Eq, Ord)

instance Show Item where
    show (Item a) = a


data ItemSet = ItemSet (Set Item) deriving (Eq, Ord)

instance Show ItemSet where
    show (ItemSet x) =
        init $ foldr ((\y old -> y ++ "," ++ old).show) "" (Set.toList x)

instance Freq ItemSet where
    frequency table (ItemSet set) =
        setCount / fromIntegral (length table) where
            setCount = fromIntegral $ count table (ItemSet set)

count :: [ItemSet] -> ItemSet -> Count
count table (ItemSet set) =
    length (filter isSuperset table) where
        isSuperset (ItemSet row) = set `Set.isSubsetOf` row

difference :: ItemSet -> ItemSet -> ItemSet
difference (ItemSet set1) (ItemSet set2) = ItemSet (Set.difference set1 set2)

empty :: ItemSet
empty = ItemSet (Set.fromList [])

data Rule = Rule ItemSet ItemSet deriving (Eq)

instance Show Rule where
    show (Rule a b) = show a ++ "," ++ show b

instance Freq Rule where
    frequency table (Rule (ItemSet set1) (ItemSet set2)) = frequency table $
        ItemSet (set1 `Set.union` set2)

confidence :: [ItemSet] -> Rule -> Confidence
confidence table (Rule x y) = frequency table (Rule x y) / frequency table x

lift :: [ItemSet] -> Rule -> Lift
lift table (Rule x y) = confidence table (Rule x y) / frequency table y
