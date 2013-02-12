

module Main where

import qualified Data.Map as M
import qualified Data.List as L

-- import Debug.Trace


import Genetic
import Polynome


pretty :: [Term] -> String
pretty ts = L.intercalate "\n" $ map show ts

refToStr :: [([Double], Double)] -> String
refToStr rs = L.intercalate "\n" res
  where res = map f rs
        f (ds, v) = L.intercalate " " (map show ds) ++ " " ++ show v

main :: IO ()
main = do
  ts <- mkInitGeneration
  seed <- rollSeed
  let z = mkGenerations seed 20 ts
      func = head z
      range = [-10.0, -9.9 .. 10.0]
      g = map f range
      f x = ([x], interpret func (M.fromList [(0,x)]))
  writeFile "stf.txt" (refToStr refs)
  writeFile "values.txt" (refToStr g)
  writeFile "func.txt" (show func)
  writeFile "plot.txt" ("plot \"stf.txt\", \"values.txt\" with lines")
  print (length ts)
  print (length z)

