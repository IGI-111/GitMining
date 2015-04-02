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
  show (Item s) = "Item " ++ s

data ItemSet = ItemSet (Set Item) deriving (Eq, Ord)
instance Show ItemSet where
  show (ItemSet x) = let y = foldl (++) "" $ map ((\x -> x ++ ",") . show) $ Set.toList x
                         in "{" ++ y ++ "}"

type Support = Double
type Confidence = Double

support :: [ItemSet] -> ItemSet -> Support
support iss is = (fromIntegral $ count iss is) / (fromIntegral $ length iss)
  where
    count iss (ItemSet is) = length $ filter (Set.isSubsetOf is) $ map (\(ItemSet x) -> x) iss

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
frequentItems :: [ItemSet] -> Support -> Map Item Integer
frequentItems x y = pi where
  pi = Map.filter (>= frequency) m where
  frequency = ceiling(y * (fromIntegral $ length x)) where
  m = foldl count Map.empty x where
  count m (ItemSet is) = Set.foldl f m is where
  f m i = Map.insertWith acc i 1 m where
  acc new old = old + 1

displayFrequentItems :: Map Item Integer -> String
displayFrequentItems i = Map.foldrWithKey (\key val old -> old ++ (itemToString key val) ++ "\n" ) "" i where
	itemToString (Item str) freq = str++" ("++(show freq)++")"

-- frequentItemSets :: [ItemSet] -> Support -> Map ItemSet Integer
-- frequentItemSets x y = pi where
--   pi = Map.filter (>= frequency) candidates where
--   frequency = ceiling(y * (fromIntegral $ length x)) where
--   candidates = foldl count Map.empty x where
--   count m (ItemSet is) = Set.foldl f m is where
--   f m i = Map.insertWith acc i 1 m where
--   acc new old = old + 1


main :: IO ()
main = do
  putStrLn "Apriori Algorithm for frequent itemsets mining"
  [f, s] <- getArgs
  c <- readFile f
  case parse csvFile "(stdin)" c of
    Left e -> do putStrLn "Error parsing input: "
                 print e
    Right r -> do
      let i = frequentItems r (read s)
      putStrLn $ displayFrequentItems i

-- helper functions defined here

allItems :: [ItemSet] -> [Item]
allItems iss = Set.toList $ foldl add Set.empty iss where
  add s (ItemSet is) =  Set.union s is

candidate :: [ItemSet] -> [Item] -> [ItemSet]
candidate x y = List.nub $ [ItemSet (Set.union (Set.singleton b) a) | (ItemSet a) <- x, b <- y, Set.notMember b a]
