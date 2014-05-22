module Data where

import Network.HTTP.Base
import Network.HTTP
import CSV

-- The maximum information to be converted
csvLength :: Int
csvLength = 10

-- The prefix for every CSV table
urlPrefix :: FilePath
urlPrefix = "http://ichart.finance.yahoo.com/table.csv?s="

-- Constructing an HTML page
-- from the response body
constructHTML :: String -> String
constructHTML body =
  "<table>\n<tr>\n" ++ (constructHTMLRaw $ csv csvLength body) ++ "</table>"
  where constructHTMLRaw :: CSV -> String
        constructHTMLRaw []           = ""
        constructHTMLRaw ((x:[]):[] ) = "\t<td>" ++ x ++ "</td>\n</tr>\n"
        constructHTMLRaw ((x:[]):xss) = "\t<td>" ++ x ++ "</td>\n</tr>\n<tr>\n" ++ constructHTMLRaw     xss
        constructHTMLRaw ((x:xs):xss) = "\t<td>" ++ x ++ "</td>\n"              ++ constructHTMLRaw (xs:xss)

-- Getting the response body and code
-- for a given url
openURL :: FilePath -> IO (String, Int)
openURL path = do
  res <- simpleHTTP $ getRequest path

  case res of
    Left  err -> return ("", 400)
    Right val -> return (rspBody val, join $ rspCode val)
  where join :: (Int, Int, Int) -> Int
        join (a, b, c) = a * 100 + b * 10 + c

-- Getting the response body and code
-- for a given stock code
openCode :: String -> IO (String, Int)
openCode code =
  openURL (urlPrefix ++ code)