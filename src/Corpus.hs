{-# LANGUAGE OverloadedStrings #-}

module Corpus (fromContinuous) where

import           Data.Text (Text, isSuffixOf)

isSuffixOfMaybe :: Text -> Text -> Maybe Text
isSuffixOfMaybe x y = f (isSuffixOf x y) y
  where
    f True  t = Just t
    f False _ = Nothing

fromContinuous :: Text -> Maybe Text
fromContinuous = isSuffixOfMaybe "ing"
