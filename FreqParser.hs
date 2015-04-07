module ItemsParser (
    parseItems,
    parseItemsWithFreq
    )where
import qualified Data.Map as Map
import Data.Map(Map)
import DataModel

parseItems :: String -> Either ParseError [ItemSet]

parseItemsWithFreq :: String -> Either ParseError Map ItemSet Frequency
