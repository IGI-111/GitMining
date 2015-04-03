module Main where

import CSVParser
import Apriori
import qualified Data.Set as Set

main :: IO()
main = do
	file <- readFile "out.csv"
	case parseCSV file of
		Left _ -> putStrLn "Could not parse out.csv"
		Right val -> mapM_ (\x -> putStrLn (show x ++ "(" ++ show (count table x) ++ ")")) (concat (frequentPatterns 0.6 table)) where
			table = map (ItemSet. Set.fromList .map Item) val
