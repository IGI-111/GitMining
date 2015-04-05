import CSVParser
import FrequentPatterns
import DataModel
import qualified Data.Set as Set
import System.Environment(getArgs)
import Control.Monad(when)

main :: IO()
main = do
    args <- getArgs
    when (2 > length args)
        (error "Usage: Main <table.csv> <threshold> <outfile.csv>")
    let filename = head args
    let threshold = read $ args !! 1
    file <- readFile filename
    case parseCSV file of
        Left _ -> error "Could not parse out.csv"
        Right val -> do
            let table = map (ItemSet. Set.fromList .map Item) val
            let freqPats = concat (frequentPatterns threshold table)
            mapM_ (\x -> putStrLn (show x ++ "(" ++ show (count table x) ++ ")")) freqPats
            when (length args > 2) $
                writeFile (args !! 2) $ formatToCSV freqPats

formatToCSV :: [ItemSet] -> String
formatToCSV = foldr (\x old -> old ++ formatRow x ++ "\n") "" where
    formatRow (ItemSet set) = init $ Set.foldr (\x old -> old ++ show x ++ ",") "" set
