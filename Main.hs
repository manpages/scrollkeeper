-- |
-- Generates executable for scrollkeeper

module Main where

import           Scrollkeeper.Runners (liftp, run)
import           Scrollkeeper.Types

main :: IO ()
main = do
  r <- raw
  putStrLn $ show $ run (liftp stuff) r
