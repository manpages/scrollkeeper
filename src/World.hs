module World (mkWorld) where

import           Data.Maybe          (fromJust)
import           Data.Text           (unpack, words)
import           Data.Time.Format    (defaultTimeLocale, parseTimeM)
import           Data.Time.LocalTime (LocalTime (..))
import           Prelude             hiding (putStrLn, readFile, words)
import           Types

-- | Takes a notes file split into lines and parses those into 'World' type.
mkWorld :: Lines -> World
mkWorld = mkWorldDo []

-- | Specifies expected format of the date string.
-- Right expects a string formatted as a Date header in an E-mail.
-- Basically a configuration option, should be configured eventually.
formatString :: String
formatString = "Date: %a, %d %b %Y %T %z"

-- | Extracts a date string to 'LocalTime'. Throws error if date is malformed.
parseDate :: String -> LocalTime
parseDate x = fromJust $ (parseTimeM True defaultTimeLocale formatString x)

-- | Takes accumulator and the rest of the lines and recursively builds up the world.
-- By looking ahead into the rest of the lines by two, extracting date and text of a note
-- and storing parsed date as 'LocalTime' and note split into words.
--
-- First argument is accumulator, second — the rest of the lines.
mkWorldDo :: World -> Lines -> World
mkWorldDo a (text : date : xs) = mkWorldDo ((parseDate $ unpack date, words text) : a) xs
mkWorldDo a _                  = a
