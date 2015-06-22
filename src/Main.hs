{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Text    (Text, pack)
import           Data.Text.IO (hGetLine, putStrLn)
import           Prelude      hiding (putStrLn, readFile)
import           System.IO    (Handle, IOMode (ReadMode), hIsEOF, openFile)

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
  h  <- openFile "sample.data" ReadMode
  xs <- lineHandler h id
  putStrLn $ pack $ show $ xs
