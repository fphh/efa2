{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module EFA.IO.CSVParser (
  csvFile,
  csvFileWithHeader, cellContent,
  Header, Name(..), TrimmedName(..), Transpose(..),
  Cells, CellsParser, structuredCell,
  csvFileWithStructure,
  ) where


-- Geklaut und adaptiert aus Real World Haskell, Kapitel 16

import Text.ParserCombinators.Parsec

import qualified EFA.IO.Parser as EFAParser
import EFA.IO.Parser (Sequence, eol)

import qualified Data.List as List
import Control.Applicative (liftA2, liftA3, (<*))
import Data.String.HT (trim)
import Data.Tuple.HT (mapPair, mapTriple)


-- | Takes a separator character.
csvFile ::
  (Sequence table, Sequence line) =>
  Char -> Parser (table (line String))
csvFile sepChar = EFAParser.endBy (line sepChar) eol

line ::
  Sequence line =>
  Char -> Parser (line String)
line sepChar = do
  -- remove separators from the beginning of a line
  skipMany (char sepChar)
  EFAParser.sepBy (cell sepChar) (char sepChar)


type Interpret a = String -> Either String a

cellContent :: (Read a) => Interpret a
cellContent str =
  case reads str of
    [(a,"")] -> Right a
    _ -> Left $ "could not parse cell content: " ++ show str

interpret :: Interpret a -> Parser String -> Parser a
interpret intp p = do
  str <- p
  case intp str of
    Left msg -> fail msg
    Right a -> return a


csvFileWithHeader ::
  (Sequence line) =>
  line (Interpret header) ->
  Interpret a ->
  Char -> Parser (line header, [line a])
csvFileWithHeader intpHeader intpData sepChar = do
  hdrLine <- headerLine intpHeader sepChar <* eol
  body <- endBy (fixedLine intpData hdrLine sepChar) eol
  return (hdrLine, body)

headerLine ::
  Sequence line =>
  line (Interpret header) -> Char -> Parser (line header)
headerLine intps sepChar = do
  -- remove separators from the beginning of a line
  skipMany (char sepChar)
  EFAParser.sepByVar (fmap (flip interpret $ cell sepChar) intps) (char sepChar)

fixedLine ::
  (Sequence line) =>
  Interpret a -> line void -> Char -> Parser (line a)
fixedLine intp n sepChar = do
  -- remove separators from the beginning of a line
  skipMany (char sepChar)
  EFAParser.sepByMatch n (interpret intp $ cell sepChar) (char sepChar)


class
  (CellsParser cells, CellsParser (Header cells), Transpose cells) =>
    Cells cells where
  type Header cells :: *

class CellsParser cells where
  structuredCell :: Char -> Parser cells

class Transpose cells where
  type Transposed cells :: *
  transpose :: [cells] -> Transposed cells


instance Cells Integer where
  type Header Integer = Name

instance CellsParser Integer where
  structuredCell = interpret cellContent . cell

instance Transpose Integer where
  type Transposed Integer = [Integer]
  transpose = id


instance Cells Double where
  type Header Double = Name

instance CellsParser Double where
  structuredCell = interpret cellContent . cell

instance Transpose Double where
  type Transposed Double = [Double]
  transpose = id


newtype Name = Name String deriving (Show, Eq)

instance Cells Name where
  type Header Name = Name

instance CellsParser Name where
  structuredCell = fmap Name . cell

instance Transpose Name where
  type Transposed Name = [Name]
  transpose = id


newtype TrimmedName = TrimmedName String deriving (Show, Eq)

instance Cells TrimmedName where
  type Header TrimmedName = Name

instance CellsParser TrimmedName where
  structuredCell = fmap (TrimmedName . trim) . cell

instance Transpose TrimmedName where
  type Transposed TrimmedName = [TrimmedName]
  transpose = id


instance Cells a => Cells [a] where
  type Header [a] = [Name]

instance CellsParser a => CellsParser [a] where
  structuredCell sepChar =
    sepBy (structuredCell sepChar) (char sepChar)

instance Transpose [a] where
  type Transposed [a] = [[a]]
  transpose = List.transpose


instance (Cells a, Cells b) => Cells (a,b) where
  type Header (a,b) = (Header a, Header b)

instance (CellsParser a, CellsParser b) => CellsParser (a,b) where
  structuredCell sepChar =
    liftA2 (,)
      (structuredCell sepChar)
      (char sepChar >> structuredCell sepChar)

instance (Transpose a, Transpose b) => Transpose (a,b) where
  type Transposed (a,b) = (Transposed a, Transposed b)
  transpose = mapPair (transpose, transpose) . unzip


instance (Cells a, Cells b, Cells c) => Cells (a,b,c) where
  type Header (a,b,c) = (Header a, Header b, Header c)

instance (CellsParser a, CellsParser b, CellsParser c) => CellsParser (a,b,c) where
  structuredCell sepChar =
    liftA3 (,,)
      (structuredCell sepChar)
      (char sepChar >> structuredCell sepChar)
      (char sepChar >> structuredCell sepChar)

instance (Transpose a, Transpose b, Transpose c) => Transpose (a,b,c) where
  type Transposed (a,b,c) = (Transposed a, Transposed b, Transposed c)
  transpose = mapTriple (transpose, transpose, transpose) . unzip3


csvFileWithStructure ::
  (Cells cells) =>
  Char -> Parser (Header cells, [cells])
csvFileWithStructure sepChar =
  liftA2 (,)
    (structuredCell sepChar <* eol)
    (endBy (structuredCell sepChar) eol)


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
