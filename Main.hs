module Main where

import CSVParser
import Apriori
import qualified Data.Set as Set
import System.Environment (getArgs)

main :: IO()
main = do
	args <- getArgs
	let filename = head args
	let threshold = read $ last args
	file <- readFile filename
	case parseCSV file of
		Left _ -> putStrLn "Could not parse out.csv"
		Right val -> mapM_ (\x -> putStrLn (show x ++ "(" ++ show (count table x) ++ ")")) (concat (frequentPatterns threshold table)) where
			table = map (ItemSet. Set.fromList .map Item) val
