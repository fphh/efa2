
module Main where


import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import Control.Monad.Error

import EFA2.Term.Equation
import EFA2.Term.Horn
import EFA2.Term.DirEquation
import EFA2.Term.EqInterpreter

import EFA2.Graph.Graph
import EFA2.Graph.DependencyGraph

import EFA2.Utils.Utils
import EFA2.Graph.SignalGraph
import EFA2.Signal.SignalAnalysis
import EFA2.Signal.SignalData

import EFA2.Display.FileSave
import EFA2.Display.DrawGraph

import EFA2.Example.Dreibein
import EFA2.Example.Linear
import EFA2.Example.Loop
import EFA2.Example.Circular
import EFA2.Example.Vierbein



main :: IO ()
main = do
  let given = give [ Energy (PowerIdx 2 4) ] -- , Energy 5 4 := Given (Energy 5 4) ]
      penv (PowerIdx 2 4) = return [1.4]
      penv idx = error (show idx)

      (g, sigs) = vierbein
      depg = makeDependencyGraph g given
      ho = hornOrder depg given
      xenv = mkMXEnv g sigs
      eenv = mkMEtaEnv g sigs 
      dirEqs = directEquations ho
      inTs :: [InTerm Abs]
      inTs = toInTerms dirEqs
  writeTopology g
  writeDependencyGraph g given


  putStrLn (hornsToStr $ makeHornFormulae depg given)

  putStrLn (showEqTerms $ hornOrder depg given)
  putStrLn ""
  putStrLn (showInTerms inTs)
  putStrLn (show $ solveInTerms penv eenv xenv inTs)
