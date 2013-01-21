

module Main where

import EFA.IO.ASCIIImport (modelicaASCIIImport)


main :: IO ()
main = do
  rec <- modelicaASCIIImport "test.asc"
  print rec