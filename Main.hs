module Main where

import CSVParser
import Apriori
import qualified Data.Set as Set
import System.Environment (getArgs)
import Control.Monad

main :: IO()
main = do
	args <- getArgs
	when (2 /= length args) (error "Usage: Main <file.csv> <threshold>")
	let filename = head args
	let threshold = read $ last args
	file <- readFile filename
	case parseCSV file of
		Left _ -> error "Could not parse out.csv"
		Right val -> mapM_ (\x -> putStrLn (show x ++ "(" ++ show (count table x) ++ ")")) (concat (frequentPatterns threshold table)) where
			table = map (ItemSet. Set.fromList .map Item) val
