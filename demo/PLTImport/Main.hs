

module Main where

import EFA.IO.PLTImport (modelicaPLTImport)
import EFA.Signal.Record (SignalRecord)

main :: IO ()
main = do
  rec <- modelicaPLTImport "test.plt"
  print (rec :: SignalRecord [] Double)