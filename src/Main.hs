
module Main where


import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import Control.Monad.Error

import Debug.Trace

import EFA2.Term.Equation
import EFA2.Term.Horn
import EFA2.Term.DirEquation
import EFA2.Term.EqInterpreter
import EFA2.Term.TermData

import EFA2.Graph.GraphData
import EFA2.Graph.Graph
import EFA2.Graph.DependencyGraph
import EFA2.Signal.Arith

import EFA2.Utils.Utils

--import EFA2.Display.FileSave
import EFA2.Display.DrawGraph

import EFA2.Example.SymSig
--import EFA2.Example.Dreibein
--import EFA2.Example.Linear
import EFA2.Example.Loop
--import EFA2.Example.Circular
--import EFA2.Example.Vierbein


mkGiven :: (Signal a) => [(PowerIdx, [Val])] -> ([EqTerm b], M.Map PowerIdx [a])
mkGiven xs = (give $ map (Energy . fst) xs, M.fromList (map (fmap (map toSignal)) xs))

main :: IO ()
main = do
  let --sigs :: LRPowerEnv [Val]
      sigs :: LRPowerEnv [InTerm]
      (g, sigs) = loop

      given :: [EqTerm Diff]
      (given, penv') = mkGiven [(PowerIdx 4 5, [1.8])]

      depg = makeDependencyGraph g given
      ho = hornOrder depg given
      dirEqs = directEquations ho
      inTs = toInTerms dirEqs

      xenv = mkXEnv g sigs
      eenv = mkEtaEnv g sigs
      penv = mkPowerEnv penv'

      sol = M.union penv' (solveInTerms penv' eenv xenv inTs)


  --writeTopology g
  --writeDependencyGraph g given


  putStrLn (hornsToStr $ makeHornFormulae depg given)

  putStrLn (showEqTerms $ hornOrder depg given)
  putStrLn ""
  putStrLn (show given)
  putStrLn ""
  putStrLn (showInTerms inTs)
  putStrLn (show sol)

  drawTopologyX g
  drawDependencyGraph g given
  drawTopology undefined (mkPowerEnv sol) eenv xenv g