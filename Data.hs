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
  "<table class=\"table table-bordered table-striped\">\n<tr>\n" ++ (constructHTMLRaw headCell $ take 1 csved) ++ (constructHTMLRaw cell $ tail csved) ++ "</table>"
  where csved :: CSV
        csved = csv csvLength body

        headCell :: String -> String
        headCell s = "<td><h4 class=\"text-center\">" ++ s ++ "</h4></td>"

        cell :: String -> String
        cell s = "<td><h5 class=\"text-center\">" ++ s ++ "</h5></td>"

        constructHTMLRaw :: (String -> String) -> CSV -> String
        constructHTMLRaw cfn []           = ""
        constructHTMLRaw cfn ((x:[]):xss) = "\t" ++ cfn x ++ "\n</tr>\n<tr>\n" ++ constructHTMLRaw cfn    xss
        constructHTMLRaw cfn ((x:xs):xss) = "\t" ++ cfn x ++ "\n"              ++ constructHTMLRaw cfn (xs:xss)

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