

module EFA.IO.PLTParser where

import EFA.Signal.Record (SigId(SigId))

import qualified EFA.IO.Parser as EFAParser
import EFA.IO.Parser (Sequence, number)
import Text.ParserCombinators.Parsec
import Control.Applicative ((*>), liftA2)


type Table v = (SigId, [v])

datasetToken :: Parser ()
datasetToken = (string "DataSet:") *> spaces

pltFile :: (Sequence tables, Read v) => Parser (tables (Table v))
pltFile = EFAParser.endBy table eol

table :: (Read v) => Parser (Table v)
table = liftA2 (,) (removeClutter *> fmap SigId header) (endBy dataline eol)

header :: Parser String
header = manyTill anyChar eol

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
