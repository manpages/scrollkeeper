module Types where

import           Data.Text           (Text)
import           Data.Time.LocalTime (LocalTime (..))

type Lines  = [Text]
type Words  = [Text]
type World  = [(LocalTime, Words)]
type Parsed = [(Entry, Text)]

newtype Verb = Verb Text deriving Show
newtype Fact = Fact Text deriving Show

data Entry = VerbEntry  (LocalTime, Verb)
           | FactEntry  (LocalTime, Fact)
           | EmptyEntry (LocalTime)
           deriving Show
