import CSVParser
import DataModel
import ExtractRules
import qualified Data.Set as Set
import System.Environment(getArgs)
import Control.Monad(when)

main :: IO()
main = do
    args <- getArgs
    when (3 > length args) (error "Usage: phase2 table.csv frequents.csv threshold [rules.csv]")
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
                    rules =  extractRules threshold table freqPats
                    output = formatToCSV rules

formatToCSV :: [Rule] -> String
formatToCSV = foldr (\x old -> old ++ formatRow x ++ "\n") "" where
    formatRow (Rule x y) = show x ++ ", ," ++ show y
