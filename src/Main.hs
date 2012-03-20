
module Main where


import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import EFA2.Term.Term
import EFA2.Term.EquationOrder
import EFA2.Term.Horn

import EFA2.Graph.Graph
import EFA2.Utils.Utils
import EFA2.Signal.SignalGraph
import EFA2.Display.FileSave

import EFA2.Example.Dreibein
import EFA2.Example.Linear
import EFA2.Example.Loop
import EFA2.Example.Circular


main :: IO ()
main = do
  let given = S.fromList [Energy 2 2]
      (g, sigs) = dreibein
      depg = makeDependencyGraph g
      fs = graphToHorn (makeDependencyGraph g)
  writeTopology g
  writeDependencyGraph g
  print sigs
  print (makeEtaEnv g sigs)

  --putStrLn (termsStr $ makeEquations g input (Energy 3 2))

  putStrLn (hornsToStr $ makeHornFormulae given depg)

  putStrLn (termsStr $ hornOrder given depg)
