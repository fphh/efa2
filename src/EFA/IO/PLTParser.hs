

module EFA.IO.PLTParser where

import EFA.Signal.Record (SigId(SigId))

import EFA.IO.Parser (number, eol)
import Text.ParserCombinators.Parsec
import Control.Applicative ((*>), liftA2)
import Control.Monad.HT (void)


type Tables v = ([v], [Table v])
type Table v = (SigId, [v])

datasetToken :: Parser ()
datasetToken = string "DataSet:"  *>  spaces

pltFile :: (Read v) => Parser (Tables v)
pltFile = do
   removeClutter
   liftA2 (,) timeTable $ eol *> endBy dataTable eol

timeTable :: (Read v) => Parser [v]
timeTable = datasetToken *> string "time\n" *> table

dataTable :: (Read v) => Parser (Table v)
dataTable = liftA2 (,) (datasetToken *> fmap SigId name) table

table :: (Read v) => Parser [v]
table = endBy (fmap snd dataline) eol

name :: Parser String
name = manyTill anyChar eol

dataline :: (Read v) => Parser (v,v)
dataline = liftA2 (,) number (string ", " *> number)

removeClutter :: Parser ()
removeClutter = void $ manyTill line (string "\n")

line :: Parser String
line = manyTill (noneOf "\n\r") eol
