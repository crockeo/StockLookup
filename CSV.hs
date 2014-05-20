module CSV where

import Text.ParserCombinators.Parsec

type CSV = [[String]]

-- A single cell
_cell :: Parser String
_cell = many $ noneOf ",\n\r"

-- A single line
_line :: Parser [String]
_line = sepBy _cell (char ',')

-- The whole csv file
csvParser :: Parser [[String]]
csvParser = sepBy _line (oneOf "\n\r")

-- Parsing out the csv
csv :: String -> [[String]]
csv s =
  case parse csvParser "csv" s of
    Left  err -> [[show err]]
    Right val -> val