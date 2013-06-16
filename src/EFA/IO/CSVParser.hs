

module EFA.IO.CSVParser (csvFile, csvFileWithHeader) where


-- Geklaut und adaptiert aus Real World Haskell, Kapitel 16

import Text.ParserCombinators.Parsec

import qualified EFA.IO.Parser as EFAParser
import EFA.IO.Parser (Sequence, eol)

import Control.Applicative ((<*))


-- | Takes a separator character.
csvFile ::
  (Sequence table, Sequence line) =>
  Char -> Parser (table (line String))
csvFile sepChar = EFAParser.endBy (line sepChar) eol

csvFileWithHeader ::
  (Sequence line) =>
  Char -> Parser (line String, [line String])
csvFileWithHeader sepChar = do
  headerLine <- line sepChar <* eol
  body <- endBy (fixedLine headerLine sepChar) eol
  return (headerLine, body)

line ::
  Sequence line =>
  Char -> Parser (line String)
line sepChar = do
  skipMany (char sepChar) -- remove separators from the beginning of a line
  EFAParser.sepBy (cell sepChar) (char sepChar)

fixedLine ::
  (Sequence line) =>
  line a -> Char -> Parser (line String)
fixedLine n sepChar = do
  skipMany (char sepChar) -- remove separators from the beginning of a line
  EFAParser.sepByMatch n (cell sepChar) (char sepChar)

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
