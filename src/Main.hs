{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Text    (Text, pack)
import           Data.Text.IO (hGetLine, putStrLn)
import           Prelude      hiding (putStrLn, readFile, words)
import           System.IO    (Handle, IOMode (..), hIsEOF, openFile)
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

main :: IO ()
main = do
  h  <- openFile "real.data" ReadMode
  xs <- lineHandler h id
  putStrLn $ pack $ show $ mkWorld xs
