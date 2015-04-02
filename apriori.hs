module Main where

import System.Environment
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

-- data structures defined here

data Item = Item String deriving (Eq, Ord)
instance Show Item where
  show (Item s) = s --"Item " ++ s

data ItemSet = ItemSet (Set Item) deriving (Eq, Ord)
instance Show ItemSet where
  show (ItemSet x) = let y = foldl (++) "" $ map ((\x -> " " ++ x) . show) $ Set.toList x
                         in y

type Support = Double
type Confidence = Double
type Frequency = Int

frequency :: [ItemSet] -> ItemSet -> Frequency
frequency iss (ItemSet is) = length $ filter (Set.isSubsetOf is) $ map (\(ItemSet x) -> x) iss

support :: [ItemSet] -> ItemSet -> Support
support iss is = (fromIntegral $ frequency iss is) / (fromIntegral $ length iss)

confidence :: [ItemSet] -> ItemSet -> Item -> Confidence
confidence iss i@(ItemSet is) j = (support iss $ ItemSet $ Set.union is $ Set.singleton j) / (support iss i)

-- parse CSV

csvFile = endBy line eol
line = do
  cells <- sepBy cell $ char ','
  return $ ItemSet $ Set.fromList cells
cell = do
  c <- many $ noneOf ",\r\n"
  return $ Item $ c
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

parseCSV :: String -> Either ParseError [ItemSet]
parseCSV input = parse csvFile ("unknown") input

-- frequent items
frequentItems :: [ItemSet] -> Support -> [Item]
frequentItems x y = Set.elems $ Map.keysSet pi where
  pi = Map.filter (>= frequency) m where
  frequency = ceiling(y * (fromIntegral $ length x)) where
  m = foldl count Map.empty x where
  count m (ItemSet is) = Set.foldl f m is where
  f m i = Map.insertWith acc i 1 m where
  acc new old = old + 1


displayFrequentItems :: Map ItemSet Frequency -> String
displayFrequentItems i = Map.foldrWithKey (\key val old -> old ++ (itemToString key val) ++ "\n" ) "" i where
	itemToString is freq = "("++(show freq)++")" ++ (show is)

frequentItemSets :: [ItemSet] -> [ItemSet] -> Support -> Map ItemSet Frequency
frequentItemSets r candidates threshold = Map.fromList $ map (\c -> (c, frequency r c)) filtered
	where filtered = filter (\c -> support r c >= threshold) candidates

main :: IO ()
main = do
  putStrLn "Apriori Algorithm for frequent itemsets mining"
  [f, s] <- getArgs
  c <- readFile f
  case parse csvFile "(stdin)" c of
    Left e -> do putStrLn "Error parsing input: "
                 print e
    Right r -> do
      let is = frequentItems r (read s)
      let iss = frequentItemSets r (candidates is) (read s)
      putStrLn $ displayFrequentItems iss

-- helper functions defined here

allItems :: [ItemSet] -> [Item]
allItems iss = Set.toList $ foldl add Set.empty iss where
  add s (ItemSet is) =  Set.union s is

candidates :: [Item] -> [ItemSet]
candidates x = map (\old -> ItemSet (Set.fromList old)) (powerset x)

powerset       :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = xss /\/ map (x:) xss
	where xss = powerset xs

(/\/)        :: [a] -> [a] -> [a]
[]     /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)
