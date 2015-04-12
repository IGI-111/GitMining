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
    when (3 > length args) (error "Usage: phase3 table.csv rules.csv count [bestRules.csv]")
    let bestRuleCount = read $ args !! 2
    tableFile <- readFile $ head args
    rulesFile <- readFile $ args !! 1
    case parseCSV tableFile of
        Left _ -> error "Could not parse table"
        Right tableFileContent ->
            case parseCSV rulesFile of
                Left _ -> error "Could not parse frequent patterns"
                Right rulesFileContent -> do
                    print $ output
                    when (length args > 3) $
                        writeFile (args !! 3) output
                    where
                    table = map (ItemSet. Set.fromList .map Item) tableFileContent
                    rules = map ruleFromRow rulesFileContent
                    bestRules = take bestRuleCount $ List.sortBy (\x y -> compare (lift table y) (lift table x)) rules
                    output = formatToCSV bestRules

formatToCSV :: [Rule] -> String
formatToCSV = foldr (\x old -> old ++ formatRow x ++ "\n") "" where
    formatRow (Rule x y) = show x ++ ", ," ++ show y

ruleFromRow :: [String] -> Rule
ruleFromRow columns = Rule item1 item2
    where
    item1 = ItemSet $ Set.fromList $ map Item $ takeWhile (/= " ") columns
    item2 = ItemSet $ Set.fromList $ map Item $ tail $ dropWhile (/= " ") columns
