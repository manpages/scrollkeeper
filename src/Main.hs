{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Category    ((>>>))
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

processWorld :: World -> [Entry]
processWorld = (<$>) f
  where
    f :: (LocalTime, Words) -> Entry
    f (tau, (v : ws)) = g (tau, (v:ws)) (fromContinuous v)
    f (tau, _)        = EmptyEntry tau
    g :: (LocalTime, Words) -> Maybe Text -> Entry
    g (t, (_:ws)) (Just x)    = VerbEntry (t, [Verb (x, ws)])
    g (t,  ws)    _           = FactEntry (t, [Fact (unwords ws)])

ppShow' :: Show a => [a] -> String
ppShow' x = concat (map show >>> map ("\n"++) $ x)

main :: IO ()
main = do
  h  <- openFile "real.data" ReadMode
  xs <- lineHandler h id
  putStrLn $ pack $ ppShow' $ processWorld $ mkWorld xs
