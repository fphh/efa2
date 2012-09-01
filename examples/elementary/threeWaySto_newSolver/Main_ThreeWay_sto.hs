
module Main where

import qualified Data.Map as M

import EFA2.Topology.Topology
import EFA2.Topology.TopologyData

import EFA2.Solver.Equation
import EFA2.Solver.EquationOrder

import EFA2.Interpreter.Env
import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.Arith

import EFA2.Display.DrawGraph

import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData
import EFA2.Signal.Signal (PSigL, TSigL)


topo :: Topology
topo = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, Source), (1, Crossing), (2, Sink), (3, Storage 0)]
        edges = [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel)]


mkSig :: Int -> ([Val] -> PSigL)
mkSig n = S.fromList . concat . replicate n


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

      time :: TSigL
      time = S.fromList ([0, 0] ++ take 20 [1..])

      pMap =  M.fromList [ (PPosIdx 0 1, mkSig n (s01 ++ s01')),
                           (PPosIdx 1 0, mkSig n (s10 ++ s10')), 
                           (PPosIdx 1 2, mkSig n (s12 ++ s12')),
                           (PPosIdx 2 1, mkSig n (s21 ++ s21')),
                           (PPosIdx 1 3, mkSig n (s13 ++ s13')),
                           (PPosIdx 3 1, mkSig n (s31 ++ s31')) ]

      (sqEnvs, sqTopo) = makeSequence (PowerRecord time pMap) topo 

      --storage0 = PowerIdx (-1) 0 24 25
      storage0 = PowerIdx (-1) 0 16 17

      (sqEnvs', ts') = makeAllEquations sqTopo (map g sqEnvs) -- { recordNumber = SingleRecord 0 })
      g x = x { recordNumber = SingleRecord 0 }
      sigs = powerMap sqEnvs'
      ts = [give storage0] ++ ts'

      envs = sqEnvs' { recordNumber = SingleRecord 0,
                       powerMap = M.insert storage0 (S.fromList [3.0]) (M.map (S.map abs) sigs),
                       fetaMap = M.singleton (FEtaIdx 3 0 15 13) (S.map (const 0.4)) }


      gd = map (eqToInTerm envs) (toAbsEqTermEquations $ order ts)

      --res :: Envs [Val]
      res = interpretFromScratch (SingleRecord 0) 1 gd


  print sqEnvs'
  putStrLn (showInTerms gd)

  --putStrLn (show $ length gd)
  drawTopology sqTopo res
