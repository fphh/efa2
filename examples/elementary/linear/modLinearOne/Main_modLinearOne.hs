
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
import EFA2.Term.Solver
import EFA2.Term.Env

import EFA2.Graph.GraphData
import EFA2.Graph.Graph
import EFA2.Graph.DependencyGraph
import EFA2.Signal.Arith

import EFA2.Utils.Utils
import EFA2.Signal.Sequence
import EFA2.IO.Import

--import EFA2.Display.FileSave
import EFA2.Display.DrawGraph

import EFA2.Example.SymSig


-- define topology 
-- g' :: TheGraph [Val]
g' :: Gr NLabel ELabel
g' = mkGraph (makeNodes no) (makeEdges no) 
  where no = [0..1]
        

main :: IO ()
main = do
  rec@(Record time sigMap) <- modelicaCSVImport "./linear_res.csv"
  let 
      sigs :: LRPowerEnv [Val]
      sigs (PowerIdx 0 1) = return $ sigMap M.! (SigId "eflow1.u")
      sigs (PowerIdx 1 0) = return $ sigMap M.! (SigId "eflow2.u")
      g :: TheGraph [Val]
      g = TheGraph g' (signal sigs)
      given = [(PowerIdx 0 1, [2.2]) ]
      gvs :: [EqTerm Abs]
      penv' :: PowerMap [Val]
      (gvs, penv') = mkGiven given
      depg = makeDependencyGraph g gvs
      ho = hornOrder depg gvs
      dirEqs = directEquations ho
      inTs = toInTerms dirEqs
      
      tcg = trc g'
      f (ins, n, outs) = (n, S.toList $ S.difference (S.fromList $ nodes tcg) (S.fromList ins) )

  drawAll [ drawTopologyX g,
            drawTopologyX' (transClose g'),
            drawDependencyGraph g given,
            drawDependencyGraphTransClose g given,
            print (mapGraph f tcg) ]
  return ()
  -- drawTopology undefined g env sol
