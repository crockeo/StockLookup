module Data where

import Network.HTTP
import CSV

-- The maximum CSV length to display
maxCSVLength :: Int
maxCSVLength = 10

-- Opening a given URL
openURL :: FilePath -> IO String
openURL path =
  (simpleHTTP (getRequest path) >>= getResponseBody) >>= (return . unlines . take maxCSVLength . lines)

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
constructHTML :: CSV -> Int -> String
constructHTML ucsv cutoff =
  "<table>\n" ++ constructHTMLRaw csv ++ "</table>"
  where csv :: CSV
        csv =
          if length ucsv > cutoff
            then take cutoff ucsv
            else             ucsv

        constructHTMLRaw :: CSV -> String
        constructHTMLRaw []           = ""
        constructHTMLRaw ((x:[]):[] ) = "\t<td>" ++ x ++ "</td>\n</tr>\n"
        constructHTMLRaw ((x:[]):xss) = "\t<td>" ++ x ++ "</td>\n</tr>\n<tr>\n" ++ constructHTMLRaw     xss
        constructHTMLRaw ((x:xs):xss) = "\t<td>" ++ x ++ "</td>\n"              ++ constructHTMLRaw (xs:xss)

-- Converting a stock code into displayable HTML
codeToHTML :: String -> IO String
codeToHTML s = do
  csv <- parseCode s
  return $ constructHTML csv maxCSVLength