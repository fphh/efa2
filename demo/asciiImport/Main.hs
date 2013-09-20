-- | Demonstriert den ASCII-Import

module Main where

import EFA.IO.ASCIIImport (modelicaASCIIImport)
import EFA.Signal.Record (SignalRecord)


main :: IO ()
main = do
  rec <- modelicaASCIIImport "test.asc"
  print (rec :: SignalRecord [] Double)