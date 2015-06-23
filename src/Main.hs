{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Text           (Text, pack, unwords)
import           Data.Text.IO        (hGetLine, putStrLn)
import           Data.Time.LocalTime (LocalTime (..))
import           Prelude             hiding (putStrLn, readFile, unwords, words)
import           System.IO           (Handle, IOMode (..), hIsEOF, openFile)

import           Corpus
import           Types
import           World

lineHandler :: Handle -> (Text -> Text) -> IO Lines
lineHandler h f = do
  e <- hIsEOF h
  lineHandlerDo e h f [] where -- I don't know if we have Functorial handles, so...
    lineHandlerDo True  _  _  a = return a
    lineHandlerDo False h1 f1 a = do
      x <- hGetLine h1
      e <- hIsEOF   h1
      lineHandlerDo e h1 f1 (f1 x : a)

processWorld :: World -> Parsed
processWorld = (<$>) f
  where
    f :: (LocalTime, Words) -> (Entry, Text)
    f (tau, (v : ws)) = (g tau v (fromContinuous v), unwords ws)
    f (tau, err)      = (EmptyEntry tau,             unwords err)
    g :: LocalTime -> Text -> Maybe Text -> Entry
    g t _ (Just x)    = VerbEntry (t, Verb x)
    g t v _           = FactEntry (t, Fact v)

main :: IO ()
main = do
  h  <- openFile "real.data" ReadMode
  xs <- lineHandler h id
  putStrLn $ pack $ show $ processWorld $ mkWorld xs
