

module EFA.IO.CSVParser (csvFile) where


-- Geklaut und adaptiert aus Real World Haskell, Kapitel 16

import Text.ParserCombinators.Parsec


import EFA.Utility ((>>!))


-- | Takes a separator character.
csvFile :: Char -> Parser [[String]]
csvFile sepChar = endBy (line sepChar) eol

line :: Char -> Parser [String]
line sepChar = do
  skipMany (char sepChar) -- remove seperators from the beginning of a line
  sepBy (cell sepChar) (char sepChar)

cell :: Char -> Parser String
cell sepChar = quotedCell <|> many (noneOf (sepChar:"\n\r"))

quotedCell :: Parser String
quotedCell =
  between (char '"') (char '"' <?> "quote at end of cell") $
  many quotedChar

-- Warum funktioniert hier (>>!) nicht?
quotedChar :: Parser Char
quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
