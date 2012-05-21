
module Main where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe


import EFA2.Topology.Topology
import EFA2.Topology.TopologyData

import EFA2.Solver.Equation
import EFA2.Solver.EquationOrder

import EFA2.Interpreter.Env
import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.Arith

import EFA2.Display.DrawGraph

import EFA2.Signal.Signal

import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData
import EFA2.Display.ReportSequence
import EFA2.Display.Plot


topo :: Topology
topo = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, Source), (1, Crossing), (2, Sink), (3, Storage 0)]
        edges = [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel)]


mkSig n = sfromList . concat . replicate n

etaf x = x/(x+1)

revetaf x = (x+1)/x

sigsXYZ = M.fromList [ (PowerIdx 0 0 0 1, sfromList [2.0]) ]


main :: IO ()
main = do
  
  let s01 = [0, 2, 2, 0]
      s10 = [0, 0.8, 0.8, 0]
      s12 = [0.3, 0.3, 0.3, 0.3]
      s21 = [0.2, 0.2, 0.2, 0.2]
      s13 = [0, 0.5, 0.5, 0]
      s31 = [0, 0.25, 0.25, 0]

      s01' = [0, 0]
      s10' = [0, 0]
      s12' = [0.3, 0.3]
      s21' = [0.2, 0.2]
      s13' = [-0.3, -0.3]
      s31' = [-0.6, -0.6]
      n = 2

      time = take (length (s01 ++ s01') * n) $ [0, 0] ++ (concatMap (replicate 3) [1..])

      pMap =  M.fromList [ (PPosIdx 0 1, mkSig n (s01 ++ s01')),
                           (PPosIdx 1 0, mkSig n (s10 ++ s10')), 
                           (PPosIdx 1 2, mkSig n (s12 ++ s12')),
                           (PPosIdx 2 1, mkSig n (s21 ++ s21')),
                           (PPosIdx 1 3, mkSig n (s13 ++ s13')),
                           (PPosIdx 3 1, mkSig n (s31 ++ s31')) ]

      pRec = (PowerRecord (sfromList time) pMap)
      (sequ,sequPwrRecord) = genSequ pRec

      (sqEnvs, sqTopo) = makeSequence pRec topo 

      --storage0 = PowerIdx (-1) 0 24 25
      storage0 = EnergyIdx (-1) 0 16 17

      (sqEnvs', ts') = makeAllEquations sqTopo [envs]


      sigs = M.unions (map powerMap sqEnvs')
      dtimes = M.unions (map dtimeMap sqEnvs')
      ts = [give storage0] ++ ts'

      f x | x < 0 = -x
      f x = x
      envs = emptyEnv { dtimeMap = dtimes, 
                        powerMap = M.map (smap f) sigsXYZ,
                        fetaMap = M.fromList [ (FEtaIdx 0 0 0 1, smap etaf), (FEtaIdx 0 0 1 0, smap revetaf),
                                               (FEtaIdx 0 0 1 2, smap etaf), (FEtaIdx 0 0 2 1, smap revetaf),
                                               (FEtaIdx 0 0 1 3, smap (const 0.6)), (FEtaIdx 0 0 3 1, smap (const (1/0.6))) ],
                        energyMap = M.insert storage0 (sfromList [1.0,1.0,1.0]) M.empty   }


      gd = map (eqToInTerm envs) (order ts)

      res :: Envs UTFSig
      res = interpretFromScratch 3 gd
  --printTableToScreen show pRec

  --print sigs
  --print sqEnvs'
  putStrLn (showEqTerms ts)

  --print sqEnvs'
  --sigPlot pRec
  --sigPlot sequPwrRecord
 

  --putStrLn (showInTerms gd)

  --putStrLn (show $ length gd)
  --drawTopology sqTopo res
