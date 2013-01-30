

module Main where

import EFA.IO.CSVImport (modelicaCSVImport)
import EFA.Signal.Record (SignalRecord)

main :: IO ()
main = do
  rec <- modelicaCSVImport "test.csv"
  print (rec :: SignalRecord [] Double)