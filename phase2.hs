import CSVParser
import DataModel
import ExtractRules
import qualified Data.Set as Set
import System.Environment(getArgs)
import Control.Monad(when)

main :: IO()
main = do
    args <- getArgs
    when (2 /= length args) (error "Usage: phase2 <file.csv> <threshold>")
    let filename = head args
    let threshold = read $ last args
    file <- readFile filename
    case parseCSV file of
        Left _ -> error "Could not parse out.csv"
        Right val -> do
            let table = map (ItemSet. Set.fromList .map Item) val
            print $ extractRules threshold table
