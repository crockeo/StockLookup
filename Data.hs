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

-- Constructing a piece of HTML from the parseCode
constructHTML :: CSV -> String
constructHTML csv =
  "<table>\n" ++ constructHTMLRaw csv ++ "</table>"
  where constructHTMLRaw :: CSV -> String
        constructHTMLRaw []           = ""
        constructHTMLRaw ((x:[]):xss) = "\t<td>" ++ x ++ "</td>\n</tr>\n<tr>\n" ++ constructHTMLRaw xss
        constructHTMLRaw ((x:xs):xss) = "\t<td>" ++ x ++ "</td>\n" ++ constructHTMLRaw (xs:xss)