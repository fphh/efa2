

module EFA.IO.PLTParser where

import Text.ParserCombinators.Parsec
import Control.Applicative ((*>), liftA2)
import EFA.Signal.Record (SigId(SigId))

type Table v = (SigId, [v])

datasetToken :: Parser ()
datasetToken = (string "DataSet:") *> spaces

pltFile :: (Read v) => Parser [Table v]
pltFile = endBy table eol

table :: (Read v) => Parser (Table v)
table = liftA2 f (removeClutter *> header) (endBy dataline eol)
  where f x y = (SigId x, y)

header :: Parser String
header = manyTill anyChar eol

number :: (Read v) => Parser v
number = read `fmap` (many $ oneOf "0123456789+-eE.")

dataline :: (Read v) => Parser v
dataline = manyTill (noneOf "\n\r") (string ", ") *> number

removeClutter :: Parser String
removeClutter = manyTill anyChar datasetToken

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
