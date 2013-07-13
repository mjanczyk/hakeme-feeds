-- | Main entry point to the application.

module Main where

import Scrapper (page, pageToPosts, Post (..), )
import qualified Data.Text as T

url = T.pack "http://hakeme.pl"

-- | The main entry point.
main :: IO ()
main = do
  html <- page url
  mapM_ ((>> putStrLn "\n") . putStrLn . show ) $ pageToPosts url html
  
