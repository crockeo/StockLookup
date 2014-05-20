module Data where

import Network.HTTP
import CSV

-- Opening a given URL
openURL :: FilePath -> IO String
openURL path =
  simpleHTTP (getRequest path) >>= getResponseBody

-- Converting a URL to parsed CSV
parseURL :: FilePath -> IO CSV
parseURL path =
  openURL path >>= return . csv

-- The URL prefix
urlPrefix :: FilePath
urlPrefix = "http://ichart.finance.yahoo.com/table.csv?s="

-- Converting a stock code into parsed CSV
parseCode :: String -> IO CSV
parseCode code = parseURL $ urlPrefix ++ code