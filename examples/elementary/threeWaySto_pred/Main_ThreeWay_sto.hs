{-# LANGUAGE GADTs #-}

module Main where

import qualified Data.Map as M
import qualified Data.Vector as V
import Debug.Trace
import Data.Monoid

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
import EFA2.Signal.Signal (TC(TC), PSigL, UTFSig, Test1, (.++))
import EFA2.Signal.Typ

import EFA2.Utils.Utils


topo :: Topology
topo = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, Source), (1, Crossing), (2, Sink), (3, Storage 0)]
        edges = [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel)]

mkSig :: Int -> ([Val] -> PSigL)
mkSig n = S.fromList . concat . replicate n

etaf x = x/(x+5)
revetaf x = (x+5)/x

--etaf x = sqrt x
--revetaf x = 1/(sqrt x)

--revFunc :: (a -> a) (a -> a)
--revFunc f x = 1/(f x)

--f2 x = sqrt(x)/35
f2 x = x/(x+10)


--variation :: Topology -> Val -> [Envs UTFSig]
variation sqTopo x y = trace (showEqTerms ts'') $ interpretFromScratch (SingleRecord 0) 1 gd
  where givenEnv0 = emptyEnv { recordNumber = SingleRecord 0,
                               dtimeMap = M.fromList [ (DTimeIdx 0 0, S.fromList [1.0] :: UTFSig) ],
                               powerMap = M.fromList [ (PowerIdx 0 0 3 1, S.fromList [x]),
                                                       (PowerIdx 0 0 2 1, S.fromList [0.6]) ],
                               fetaMap =  M.fromList [ (FEtaIdx  0 0 0 1, S.map (const 0.4)), (FEtaIdx 0 0 1 0, S.map (const 0.6)),
                                                       (FEtaIdx  0 0 1 2, S.map (const 0.7)), (FEtaIdx 0 0 2 1, S.map (const 0.7)),
                                                       (FEtaIdx  0 0 1 3, S.map f2), (FEtaIdx 0 0 3 1, S.map f2) ] }

        givenEnv1 = emptyEnv { recordNumber = SingleRecord 0,
                               dtimeMap = M.fromList [ (DTimeIdx 1 0, S.fromList [1.0]) ],
                               energyMap = M.fromList [ (EnergyIdx 1 0 3 1, S.fromList [x] :: UTFSig) ],
                               powerMap =  M.fromList [ (PowerIdx 1 0 2 1, S.fromList [0.6]) ],
                               fetaMap =   M.fromList [ (FEtaIdx 1 0 0 1, S.map (const 0.4)), (FEtaIdx 1 0 1 0, S.map (const 0.6)),
                                                        (FEtaIdx 1 0 1 2, S.map (const 0.7)), (FEtaIdx 1 0 2 1, S.map (const 0.7)),
                                                        (FEtaIdx 1 0 1 3, S.map f2), (FEtaIdx 1 0 3 1, S.map f2) ] }

        givenEnv2 = emptyEnv { recordNumber = SingleRecord 0,
                               dtimeMap = M.fromList [ (DTimeIdx 2 0, S.fromList [1.0]) ],
                               energyMap = M.fromList [ (EnergyIdx 2 0 3 1, S.fromList [x] :: UTFSig) ],
                               powerMap =  M.fromList [ (PowerIdx 2 0 2 1, S.fromList [0.6]) ],
                               fetaMap =   M.fromList [ (FEtaIdx 2 0 0 1, S.map (const 0.4)), (FEtaIdx 2 0 1 0, S.map (const 0.6)),
                                                        (FEtaIdx 2 0 1 2, S.map (const 0.7)), (FEtaIdx 2 0 2 1, S.map (const 0.7)),
                                                        (FEtaIdx 2 0 1 3, S.map f2), (FEtaIdx 2 0 3 1, S.map f2) ] }

        givenEnv3 = emptyEnv { recordNumber = SingleRecord 0,
                               dtimeMap = M.fromList [ (DTimeIdx 3 0, S.fromList [1.0]) ],
                               energyMap = M.fromList [ (EnergyIdx 3 0 3 1, S.fromList [x] :: UTFSig) ],
                               powerMap =  M.fromList [ (PowerIdx 3 0 2 1, S.fromList [0.6]) ],
                               fetaMap =   M.fromList [ (FEtaIdx 3 0 0 1, S.map (const 0.4)), (FEtaIdx 3 0 1 0, S.map (const 0.6)),
                                                        (FEtaIdx 3 0 1 2, S.map (const 0.7)), (FEtaIdx 3 0 2 1, S.map (const 0.7)),
                                                        (FEtaIdx 3 0 1 3, S.map f2), (FEtaIdx 3 0 3 1, S.map f2) ] }



        (sqEnvs, ts') = makeAllEquations sqTopo [givenEnv0, givenEnv1, givenEnv2, givenEnv3]


        storage0 = EnergyIdx (-1) 0 17 16
        dtime0 = DTimeIdx (-1) 0
        ts = [give storage0, give dtime0] ++ ts'

        sqEnvs' = sqEnvs { dtimeMap = M.insert (DTimeIdx (-1) 0) (S.fromList [1.0]) (dtimeMap sqEnvs),
                           energyMap = M.insert storage0 (S.fromList [3.0]) (energyMap sqEnvs) }

        ts'' = toAbsEquations $ order ts
        gd = map (eqToInTerm sqEnvs') ts''


getEnergy :: EnergyIdx -> [Envs UTFSig] -> Test1 (Typ A F Tt) Val
getEnergy idx envs = S.setTypeTestRow $ S.setType $ mconcat $ map (flip safeLookup idx) pm
  where pm = map energyMap envs


--fconv :: [Test1 (Typ A F Tt) Val] -> Test2 (Typ A F Tt) Val
fconv :: [TC t t1 a] -> TC s t (V.Vector a)
fconv xs = TC (V.fromList $ map f xs)
  where f (TC ys) = ys

main :: IO ()
main = do
  let s01 = [0, 2, 2, 0, 0, 0]
      s10 = [0, 0.8, 0.8, 0, 0, 0]
      s12 = [0.3, 0.3, 0.3, 0.3, 0.3, 0.3]
      s21 = [0.2, 0.2, 0.2, 0.2, 0.2, 0.2]
      s13 = [0, 0.5, 0.5, 0, -0.3, -0.3]
      s31 = [0, 0.25, 0.25, 0, -0.6, -0.6]

      n = 2
      --l = fromIntegral $ length $ replicate n (s01 ++ s01')
      --time = [0, 0] ++ (concatMap (replicate 3) [1.0 .. l])
      time = take 13 [0 ..]


      pMap =  M.fromList [ (PPosIdx 0 1, mkSig n s01 .++ (S.fromList [head s01] :: PSigL)),
                           (PPosIdx 1 0, mkSig n s10 .++ (S.fromList [head s10] :: PSigL)),
                           (PPosIdx 1 2, mkSig n s12 .++ (S.fromList [head s12] :: PSigL)),
                           (PPosIdx 2 1, mkSig n s21 .++ (S.fromList [head s21] :: PSigL)),
                           (PPosIdx 1 3, mkSig n s13 .++ (S.fromList [head s13] :: PSigL)),
                           (PPosIdx 3 1, mkSig n s31 .++ (S.fromList [head s31] :: PSigL)) ]

      pRec = PowerRecord (S.fromList time) pMap
      (_, sqTopo) = makeSequence pRec topo

      lst = [1, 2]
      etas = [0.2]
 

      res = map f (sequence [lst, etas])
      f [x, y] = variation sqTopo x y
{-
      f (x:y:_) = [(x, y, g (head $ S.toList $ m `safeLookup` (EnergyIdx 0 0 0 1))
                            (head $ S.toList $ m `safeLookup` (EnergyIdx 0 0 2 1))
                            (head $ S.toList $ m `safeLookup` (EnergyIdx 1 0 6 5))) ]
                            where m = energyMap $ variation sqTopo x y
-}

      g e0001 e0021 e1021 = (e0021 + e1021)/e0001
      -- env = map (\(x:y:_) -> variation sqTopo x y) (sequence [[1.07, 1.08 .. 1.1], [0.7]])


  putStrLn (show time)
  putStrLn (show pRec) 
  putStrLn (show sqTopo)
  drawTopologyX' sqTopo
--  plotMesh3d [] [] res
  print (head res)
  mapM_ (drawTopology sqTopo) res
  drawTopology sqTopo (head res)
