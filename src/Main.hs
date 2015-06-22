{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Maybe          (fromJust)
import           Data.Text           (Text, pack, unpack, words)
import           Data.Text.IO        (hGetLine, putStrLn)
import           Data.Time.Format    (defaultTimeLocale, formatTime, parseTimeM)
import           Data.Time.LocalTime (LocalTime (..))
import           Prelude             hiding (putStrLn, readFile, words)
import           System.IO           (Handle, IOMode (..), hIsEOF, openFile)

type Lines = [Text]
type Words = [Text]
type World = [(LocalTime, Words)]

mkWorld :: Lines -> World
mkWorld = mkWorldDo []

formatString :: String
formatString = "Date: %a, %d %b %Y %T %z"

parseDate :: String -> LocalTime
parseDate x = fromJust $ (parseTimeM True defaultTimeLocale formatString x)

mkWorldDo :: World -> Lines -> World
mkWorldDo a (text : date : xs) = mkWorldDo ((parseDate $ unpack date, words text) : a) xs
mkWorldDo a _                  = a

lineHandler :: Handle -> (Text -> Text) -> IO [Text]
lineHandler h f = do
  e <- hIsEOF h
  lineHandlerDo e h f [] where -- I don't know if we have Functorial handles, so...
    lineHandlerDo True  _  _  a = return a
    lineHandlerDo False h1 f1 a = do
      x <- hGetLine h1
      e <- hIsEOF   h1
      lineHandlerDo e h1 f1 (f1 x : a)

main :: IO ()
main = do
  h  <- openFile "real.data" ReadMode
  xs <- lineHandler h id
  putStrLn $ pack $ show $ mkWorld xs
