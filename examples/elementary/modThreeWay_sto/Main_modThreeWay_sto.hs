
module Main where


import Data.Graph.Inductive

import qualified Data.Map as M

import EFA2.Topology.Topology

import EFA2.Solver.Equation
import EFA2.Solver.EquationOrder
{-
import EFA2.Solver.Horn
import EFA2.Solver.DirEquation
import EFA2.Solver.IsVar
import EFA2.Solver.DependencyGraph
-}

import EFA2.Interpreter.Env
import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.InTerm

import EFA2.Topology.Draw

import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData
import qualified EFA2.Signal.Signal as S

import EFA2.Topology.TopologyData


topo :: Topology
topo = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, Source), (1, Crossing), (2, Sink), (3, Storage 0)]
        edges = [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel)]

{-
test :: EfaGraph () ()
test = mkGraph nodes edges
  where nodes = [(0, ()), (1, ()), (2, ()), (3, ())]
        edges = [(0, 1, ()), (1, 2, ()), (1, 3, ())]
-}

main :: IO ()
main = do  
  let s01 = [0, 1, 1, 0]
      s10 = [0, 0.4, 0.4, 0]
      s12 = [0.3, 0.3, 0.3, 0.3]
      s21 = [0.2, 0.2, 0.2, 0.2]
      s13 = [0, 0.1, 0.1, 0]
      s31 = [0, 0.05, 0.05, 0]
      s01' = [0, 0, 0]
      s10' = [0, 0, 0]
      s12' = [0.3, 0.3, 0.3]
      s21' = [0.2, 0.2, 0.2]
      s13' = [-0.3, -0.3, -0.3]
      s31' = [-0.6, -0.6, -0.6]
      n = 3

      time = S.fromList ([0, 0] ++ (concatMap (replicate 3) [1..10]))

      pMap =  M.fromList [ (PPosIdx 0 1, S.fromList $ concat $ replicate n (s01 ++ s01')),
                           (PPosIdx 1 0, S.fromList $ concat $ replicate n (s10 ++ s10')),
                           (PPosIdx 1 2, S.fromList $ concat $ replicate n (s12 ++ s12')),
                           (PPosIdx 2 1, S.fromList $ concat $ replicate n (s21 ++ s21')),
                           (PPosIdx 1 3, S.fromList $ concat $ replicate n (s13 ++ s13')),
                           (PPosIdx 3 1, S.fromList $ concat $ replicate n (s31 ++ s31')) ]

      (sqEnvs, sqTopo) = makeSequence (PowerRecord time pMap) topo

      storage0 = PowerIdx Idx.initSection 0 24 25

      (sqEnvs', ts') = makeAllEquations sqTopo (zipWith f [0..] sqEnvs)
      f x env = env { recordNumber = SingleRecord x }
      sigs = powerMap sqEnvs'
      ts = [give storage0] ++ ts'

      orderedTs = order ts
      envs = emptyEnv { powerMap = M.insert storage0 (S.fromList [3.0]) sigs }

      gd = toAbsEquations $ map (eqToInTerm envs) orderedTs

      res = interpretFromScratch (SingleRecord 0) 1 gd
      -- dirg = makeDirTopology sqTopo

  drawTopology sqTopo res
