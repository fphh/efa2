

module Main where

import qualified EFA.IO.CSVImport as CSV
import EFA.Signal.Record (SignalRecord)

type Cells = (CSV.Name, Double, CSV.Name)

test0 :: IO ()
test0 = do
  rec <- CSV.modelicaCSVImport "test.csv"
  print (rec :: SignalRecord [] Double)

test1:: IO ()
test1 = do
  rec <- CSV.fortissCSVImportStruct "fortiss.csv"
  print (rec :: (CSV.Header Cells, [Cells]))

main :: IO ()
main = test0 >> test1
