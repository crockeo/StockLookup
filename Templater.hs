module Templater where

import Text.ParserCombinators.Parsec
import Data.ByteString.Char8 (ByteString, unpack, pack)

-- Packing a ByteString
bs :: String -> ByteString
bs = pack

-- Unpacking a ByteString
unbs :: ByteString -> String
unbs = unpack

-- Searching for a valid variable name
varName :: Parser String
varName = do
  fc <- lower
  ns <- many $ (alphaNum <|> oneOf "_-")

  return $ fc : ns

-- Replacing a single marker
marker :: [(String, String)] -> Parser String
marker ss = try $ do
  string "<repme>"
  k <- varName
  string "</repme>"

  return $ find k ss
  where
    find :: String -> [(String, String)] -> String
    find s []         = ""
    find s ((k,v):xs) =
      if s == k
        then v
        else find s xs

-- Replacing every marker
allMarker :: [(String, String)] -> Parser String
allMarker ps = do
  ss <- many $ choice [marker ps,
                       count 1 anyChar]

  return $ foldl1 (++) ss

-- The application of the marker parser
replace :: [(String, String)] -> String -> String
replace ps html =
  case parse (allMarker ps) "replace" html of
    Left  err -> show err
    Right val -> val

-- Loading an HTML file
pageRaw :: [(String, String)] -> FilePath -> IO String
pageRaw ps fp =
  readFile fp >>= return . replace ps

-- Loading an HTML file with the
-- header and footer added
page :: [(String, String)] -> FilePath -> IO String
page ps fp = do
  header <- pageRaw ps "templates/header.html"
  page   <- pageRaw ps fp
  footer <- pageRaw ps "templates/footer.html"

  return $ header ++ (fixIndentation page) ++ footer
  where fixIndentation :: String -> String
        fixIndentation s =
          unlines $ map (\l -> "\t\t" ++ l) $ lines s

-- Packing the page function into
-- a ByteString
bsPage :: [(String, String)] -> FilePath -> IO ByteString
bsPage fs fp =
  page fs fp >>= return . bs

-- Simply loading a bsPage
simpleBsPage :: String -> IO ByteString
simpleBsPage name =
  bsPage [("pageName", name)] $ "templates/" ++ name ++ ".html"