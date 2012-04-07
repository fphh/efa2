
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
import EFA2.Term.DependencyGraph

--import EFA2.Graph.GraphData
--import EFA2.Graph.Graph
import EFA2.Signal.Arith

import EFA2.Utils.Utils

--import EFA2.Display.FileSave
import EFA2.Display.DrawGraph

--import EFA2.Example.SymSig

--import EFA2.Example.Dreibein
--import EFA2.Example.Linear
--import EFA2.Example.LinearOne
--import EFA2.Example.LinearX
--import EFA2.Example.LinearTwo
--import EFA2.Example.Loop
--import EFA2.Example.Circular
--import EFA2.Example.Vierbein





main :: IO ()
main = do
  let --g :: TheGraph [Val]
      --g@(TheGraph g' s) = circular
      terms :: [EqTerm]
      terms = [ PowerIdx 0 2 .= [2.2, 6, 2, 2.6 :: Val],
                EtaIdx 0 1 .= [0.1, 0.2, 0.5, 0.8 :: Val],
                mkVar (VarIdx 1) := mkVar (VarIdx 0),
                mkVar (VarIdx 0) := mkVar (PowerIdx 0 2),
                mkVar (PowerIdx 0 3) := FAbs (mkVar (PowerIdx 0 1)) (mkVar (EtaIdx 0 1)),
                mkVar (VarIdx 0) := (mkVar (PowerIdx 0 1)) :* (mkVar (EtaIdx 0 1)) ]

      depg1 = dpgDiffByAtMostOne terms
      depg2 = dpgHasSameVariable terms

      ho = hornOrder depg1

  drawAll [
    drawDependencyGraph depg1,
    drawDependencyGraph depg2,

    -- drawDependencyGraph (transClose depg),
    putStrLn (showEqTerms ho) ]


{-
      gvs :: [EqTerm]
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
-}
