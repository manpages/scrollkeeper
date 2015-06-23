module World where

import           Data.Maybe          (fromJust)
import           Data.Text           (unpack, words)
import           Data.Time.Format    (defaultTimeLocale, parseTimeM)
import           Data.Time.LocalTime (LocalTime (..))
import           Prelude             hiding (putStrLn, readFile, words)
import           Types


mkWorld :: Lines -> World
mkWorld = mkWorldDo []

formatString :: String
formatString = "Date: %a, %d %b %Y %T %z"

parseDate :: String -> LocalTime
parseDate x = fromJust $ (parseTimeM True defaultTimeLocale formatString x)

mkWorldDo :: World -> Lines -> World
mkWorldDo a (text : date : xs) = mkWorldDo ((parseDate $ unpack date, words text) : a) xs
mkWorldDo a _                  = a
