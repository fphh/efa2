

module EFA.IO.CSVParser (csvFile) where


-- Geklaut und adaptiert aus Real World Haskell, Kapitel 16

import Text.ParserCombinators.Parsec

import qualified EFA.IO.Parser as EFAParser
import EFA.IO.Parser (Sequence, eol)


-- | Takes a separator character.
csvFile ::
  (Sequence table, Sequence line) =>
  Char -> Parser (table (line String))
csvFile sepChar = EFAParser.endBy (line sepChar) eol

line ::
  Sequence line =>
  Char -> Parser (line String)
line sepChar = do
  skipMany (char sepChar) -- remove seperators from the beginning of a line
  EFAParser.sepBy (cell sepChar) (char sepChar)

cell :: Char -> Parser String
cell sepChar = quotedCell <|> many (noneOf (sepChar:"\n\r"))

quotedCell :: Parser String
quotedCell =
  between (char '"') (char '"' <?> "quote at end of cell") $
  many quotedChar

quotedChar :: Parser Char
quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')
