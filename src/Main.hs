
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

import EFA2.Graph.GraphData
import EFA2.Graph.Graph
import EFA2.Graph.DependencyGraph
import EFA2.Signal.Arith

import EFA2.Utils.Utils

--import EFA2.Display.FileSave
import EFA2.Display.DrawGraph

import EFA2.Example.SymSig

import EFA2.Example.Dreibein
import EFA2.Example.Linear
import EFA2.Example.LinearOne
import EFA2.Example.ModLinearOne
import EFA2.Example.LinearX
import EFA2.Example.LinearTwo
import EFA2.Example.Loop
import EFA2.Example.Circular
import EFA2.Example.Vierbein




main :: IO ()
main = do
  let g = modLinearOne
  -- let g = vierbein
  --     given = [(PowerIdx 0 2, [2.2]) ]

  --     env :: AbsEnv [Val]
  --     --env = symbolicDiffEnv
  --     env = environment g

  --     sol = solve g env given

  --writeTopology g
  --writeDependencyGraph g given

{-
  putStrLn (hornsToStr $ makeHornFormulae depg given)

  putStrLn (showEqTerms $ hornOrder depg given)
  putStrLn ""
  putStrLn (show given)
  putStrLn ""
  putStrLn (showInTerms inTs)
-}
  putStrLn (show sol)


  drawTopologyX g
  drawDependencyGraph g given
  drawTopology undefined g env sol
