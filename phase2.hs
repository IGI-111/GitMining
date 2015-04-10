import CSVParser
import DataModel
import ExtractRules
import qualified Data.Set as Set
import qualified Data.List as List
import System.Environment(getArgs)
import Control.Monad(when)

main :: IO()
main = do
    args <- getArgs
    when (3 > length args) (error "Usage: phase2 table.csv frequents.csv threshold [out.assoc]")
    let threshold = read $ args !! 2
    tableFile <- readFile $ head args
    freqFile <- readFile $ args !! 1
    case parseCSV tableFile of
        Left _ -> error "Could not parse table"
        Right tableFileContent ->
            case parseCSV freqFile of
                Left _ -> error "Could not parse frequent patterns"
                Right freqFileContent -> do
                    print $ zip rules $ map (lift table) rules
                    when (length args > 3) $
                        writeFile (args !! 3) output
                    where
                    freqPats = map ((ItemSet. Set.fromList .map Item) . tail) freqFileContent
                    table = map (ItemSet. Set.fromList .map Item) tableFileContent
                    rules = List.sortBy (\x y -> compare (lift table y) (lift table x)) $ extractRules threshold table freqPats
                    output = init $ foldr ((\x old -> old ++ x ++ "\n").show) "" $ take 10 rules

