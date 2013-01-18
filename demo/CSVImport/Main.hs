

module Main where

import EFA.IO.CSVImport (modelicaCSVImport)


main :: IO ()
main = do
  rec <- modelicaCSVImport "test.csv"
  print rec