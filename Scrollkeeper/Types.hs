-- |
-- Types used in scrollkeeper.

module Scrollkeeper.Types where

import           Data.Map            (Map)
import           Data.Time.Clock     (NominalDiffTime)
import           Data.Time.LocalTime (LocalTime)
import           Text.Parsec

-- | Strict record for data contained in @Dimension@s of the @Space@
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

-- | Inhabited ∞-dimensional space containing n-dimensional bodies
-- represented as @Entry@. Modelled as a list.
type Space = [Entry]

-- | Function type for functions that specify how a certain @Entry@
-- parsed by user-supplied parser should be added to the Space with
-- respect to the previously parsed @Entry@. See (???) for examples
-- of built-in adders.
--
-- This type signature might be read as follows:
--
-- An entry adder is a function that takes @Entry@ that is currently
-- stored in the registry (if any*) and the newely parsed @Entry@ and returns
-- a tuple where the first element is either inhabited @Entry@ that has
-- to be added to the @Space@ or empty @Entry@ and second element is the
-- entry that has to be stored in the registry.
--
-- You can combine adders with (.>.) function from Scrollkeeper.Extras.
--
-- * — lack of @Entry@ in the registry is modelled with an empty Map.
type EntryAdder = Entry -> Entry -> (Entry, Entry)

-- | State of parser. It's a two-registry machine storing an @Entry@
-- in the first registry while accumulated @Space@ is held in the second
-- registry and is returned after top-to-bottom evaluation.
type ParseState = (Entry, Space)

-- | Shorthand for return value of parsers.
type PE a = Either ParseError a

-- | Shorthand for NominalDiffTime.
type DTime = NominalDiffTime
