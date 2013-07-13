-- | Main entry point to the application.

module Main where

import Scrapper (page, parseAll, Post (..), )
import qualified Data.Text as T

url = "http://hakeme.pl"

-- | The main entry point.
main :: IO ()
main = do
  p <- page url
  mapM_ ((>> putStrLn "\n") . putStrLn . show ) $ parseAll url p
  
