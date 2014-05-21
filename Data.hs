module Data where

import Network.HTTP
import CSV

-- The maximum CSV length to display
maxCSVLength :: Int
maxCSVLength = 10

-- Opening a given URL
openURL :: FilePath -> IO String
openURL path =
  simpleHTTP (getRequest path) >>= getResponseBody

-- Converting a URL to parsed CSV
parseURL :: Int -> FilePath -> IO CSV
parseURL l path =
  openURL path >>= return . csv l

-- The URL prefix
urlPrefix :: FilePath
urlPrefix = "http://ichart.finance.yahoo.com/table.csv?s="

-- Converting a stock code into parsed CSV
parseCode :: Int -> String -> IO CSV
parseCode l code = parseURL l $ urlPrefix ++ code

-- Constructing a piece of HTML from the parseCode
constructHTML :: CSV -> String
constructHTML csv =
  "<table>\n<tr>\n" ++ constructHTMLRaw csv ++ "</table>"
  where constructHTMLRaw :: CSV -> String
        constructHTMLRaw []           = ""
        constructHTMLRaw ((x:[]):[] ) = "\t<td>" ++ x ++ "</td>\n</tr>\n"
        constructHTMLRaw ((x:[]):xss) = "\t<td>" ++ x ++ "</td>\n</tr>\n<tr>\n" ++ constructHTMLRaw     xss
        constructHTMLRaw ((x:xs):xss) = "\t<td>" ++ x ++ "</td>\n"              ++ constructHTMLRaw (xs:xss)

-- Converting a stock code into displayable HTML
codeToHTML :: String -> IO String
codeToHTML s =
  parseCode maxCSVLength s >>= return . constructHTML