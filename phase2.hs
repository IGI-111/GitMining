import CSVParser
import DataModel
import ExtractRules
import qualified Data.Set as Set
import System.Environment(getArgs)
import Control.Monad(when)

main :: IO()
main = do
    args <- getArgs
    when (3 > length args) (error "Usage: phase2 table.csv frequents.csv threshold [out.assoc]")
    let threshold = read $ last args
    tableFile <- readFile $ head args
    freqFile <- readFile $ args !! 1
    case parseCSV tableFile of
        Left _ -> error "Could not parse table"
        Right tableFileContent ->
            case parseCSV freqFile of
                Left _ -> error "Could not parse frequent patterns"
                Right freqFileContent -> do
                    --putStrLn output
                    print $ zip rules (map (lift table) rules)
                    when (length args > 3) $
                        writeFile (args !! 3) $ output
                    where
                        freqPats = map (ItemSet. Set.fromList .map Item) (map tail freqFileContent)
                        table = map (ItemSet. Set.fromList .map Item) tableFileContent
                        rules = extractRules threshold table freqPats
                        output = init $ foldr (\x old -> old++x++"\n") "" $ map show rules
