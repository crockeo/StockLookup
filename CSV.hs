module CSV where

import Text.ParserCombinators.Parsec

import Debug.Trace

type CSV = [[String]]

-- A single cell
_cell :: Parser String
_cell = many $ noneOf ",\n\r"

-- A single line
_line :: Parser [String]
_line = sepBy _cell (char ',')

-- The whole csv file
csvParser :: Parser CSV
csvParser = sepBy _line (oneOf "\n\r")

-- Parsing out the csv
csv :: Int -> String -> CSV
csv l s =
  case parse csvParser "csv" fs of
    Left  err -> [[show err]]
    Right val -> val
  where fs :: String
        fs = init $ unlines $ take l $ lines s