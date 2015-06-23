module Types where

import           Data.Text           (Text)
import           Data.Time.LocalTime (LocalTime (..))

type Lines = [Text]
type Words = [Text]
type World = [(LocalTime, Words)]
