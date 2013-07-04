

module Main where

import qualified EFA.IO.CSVImport as CSV
import EFA.Signal.Record (SignalRecord)
import Data.Tuple.HT (thd3)


test0 :: IO ()
test0 = do
  rec <- CSV.modelicaCSVImport "test.csv"
  print (rec :: SignalRecord [] Double)

test1:: IO ()
test1 = do
  (hd,rec) <- CSV.fortissCSVImportStruct "fortiss.csv"
  print hd
  print $ filter ((CSV.TrimmedName "PERCENT" ==) . thd3) $
     (rec :: [(CSV.Name, Double, CSV.TrimmedName)])

main :: IO ()
main = test0 >> test1
