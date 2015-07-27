-- |
-- Types used in scrollkeeper.

module Scrollkeeper.Types where

import           Data.Map            (Map)
import           Data.Time.Clock     (NominalDiffTime)
import           Data.Time.LocalTime (LocalTime)
import           Text.Parsec

-- | Strict record for data bits, contained in dimensions of the space
data Tag = S !String                 -- ^ A single string
         | W ![String]               -- ^ List of words
         | F !Float                  -- ^ Float
         | I !Int                    -- ^ Integer
         | P !(LocalTime, LocalTime) -- ^ Period
         | T !LocalTime              -- ^ Timestamp
         deriving (Show)

-- | Data parsed by scrollkeeper is stored in an ∞-dimensional @Space@,
-- dimensions are named as strings using this type.
type Dimension  = String

-- | Each entry is a map from @Dimension@ to @Tag@. You can think about it
-- as of a n-dimensional body in ∞-dimensional space.
type Entry = Map Dimension Tag

-- | ∞-dimensional space containing n-dimensional bodies as @Entry@.
-- Modelled as a list.
type Space = [Entry]

-- | Function type for functions that specify how a certain @Entry@
-- parsed by user-supplied parser should be added to the Space with
-- respect to the previously parsed @Entry@. See (???) for examples
-- of built-in adders.
--
-- This type signature might be read as follows:
--
-- An entry adder is a function that takes @Entry@ that is currently
-- stored in the registry (if any) and the newely parsed @Entry@ and returns
-- a tuple where the first element is either Just @Entry@ that has
-- to be added to the @Space@ or Nothing and second element is the
-- entry that has to be stored in the registry.
--
-- You can combine adders with (.>.) function from Scrollkeeper.Extras.
type EntryAdder = Entry -> Entry -> (Entry, Entry)

-- | State of parser. It's a three-registry machine storing an @Entry@
-- in the first registry and an @EntryAdder@ stored in the second registry.
-- accumulated @Space@ is held in the third registry and is returned after
-- top-to-bottom evaluation.
type ParseState = (Entry, Space)

-- | Shorthand for return value of parsers
type PE a = Either ParseError a

-- | Shorthand for NominalDiffTime
type DTime = NominalDiffTime
