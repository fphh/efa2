

module Main where

import qualified Data.Map as M
import qualified Data.List as L

-- import Debug.Trace

import Genetic
import Surface


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
  let z = mkGenerations seed 30 ts
      func = head z
      range = sequence [[-5, -4.9 .. 5.0], [-5.0, -4.9 .. 5.0]]
      g = map f range
      f [x, y] = ([x, y], interpret func (M.fromList [(0,x), (1, y)]))
      f _ = error "f"
  writeFile "stf.txt" (refToStr refs)
  writeFile "values.txt" (refToStr g)
  writeFile "func.txt" (show func)
  writeFile "plot.txt" ("splot \"stf.txt\", \"values.txt\" with line palette")
  print (length ts)
  print (length z)

