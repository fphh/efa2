{-# LANGUAGE GADTs #-}

module Main where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Maybe
import Data.Monoid
import Graphics.Gnuplot.Simple


import EFA2.Topology.Topology
import EFA2.Topology.TopologyData

import EFA2.Solver.Equation
import EFA2.Solver.EquationOrder

import EFA2.Interpreter.Env
import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.Arith

import EFA2.Display.DrawGraph
import EFA2.Display.Report

import EFA2.Signal.Signal
import EFA2.Signal.Data
import EFA2.Signal.Typ

import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData
import EFA2.Display.ReportSequence
import EFA2.Display.Plot
import EFA2.Utils.Utils
import EFA2.Display.DispBase
import EFA2.Display.DispTyp


topo :: Topology
topo = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, Source), (1, Crossing), (2, Sink), (3, Storage 0)]
        edges = [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel)]

mkSig :: Int -> ([Val] -> PSigL)
mkSig n = sfromList . concat . replicate n

etaf x = 1-1/(1+x)


--variation :: Topology -> Val -> [Envs UTFSig]
variation sqTopo x y = interpretFromScratch (SingleRecord 0) 1 gd
  where givenEnv0 = emptyEnv { recordNumber = SingleRecord 0,
                               dtimeMap = M.fromList [ (DTimeIdx 0 0, sfromList [1.0] :: UTFSig) ],
                               powerMap = M.fromList [ (PowerIdx 0 0 3 1, sfromList [x]),
                                                       (PowerIdx 0 0 2 1, sfromList [0.6]) ],
                               fetaMap =  M.fromList [ (FEtaIdx  0 0 0 1, smap etaf), (FEtaIdx 0 0 1 0, smap etaf),
                                                       (FEtaIdx  0 0 1 2, smap (const 0.9)), (FEtaIdx 0 0 2 1, smap (const 0.9)),
                                                       (FEtaIdx  0 0 1 3, smap (const y)), (FEtaIdx 0 0 3 1, smap (const y)) ] }


        (sqEnvs, ts') = makeAllEquations sqTopo [givenEnv0]

        storage0 = EnergyIdx (-1) 0 8 9
        dtime0 = DTimeIdx (-1) 0
        ts = [give storage0, give dtime0] ++ ts'

        sqEnvs' = sqEnvs { dtimeMap = M.insert (DTimeIdx (-1) 0) (sfromList [1.0]) (dtimeMap sqEnvs),
                           energyMap = M.insert storage0 (sfromList [3.0]) (energyMap sqEnvs) }

        gd = map (eqToInTerm sqEnvs') (order ts)


genVariationMatrix :: [a] -> [b] -> [[(a,b)]]
genVariationMatrix xs ys = map ((flip zip ys) . repeat) xs;



{-
getEnergy :: EnergyIdx -> [Envs UTFSig] -> Test1 (Typ A F Tt) Val
getEnergy idx envs = setTypeTestRow $ setType $ mconcat $ map (flip safeLookup idx) pm
  where pm = map energyMap envs


--fconv :: [Test1 (Typ A F Tt) Val] -> Test2 (Typ A F Tt) Val
fconv :: [TC t t1 a] -> TC s t (V.Vector a)
fconv xs = TC (V.fromList $ map f xs)
  where f (TC ys) = ys
-}

main :: IO ()
main = do

  let s01 = [0, 2, 2, 0, 0, 0]
      s10 = [0, 0.8, 0.8, 0, 0, 0]
      s12 = [0.3, 0.3, 0.3, 0.3, 0.3, 0.3]
      s21 = [0.2, 0.2, 0.2, 0.2, 0.2, 0.2]
      s13 = [0, 0.5, 0.5, 0, -0.3, -0.3]
      s31 = [0, 0.25, 0.25, 0, -0.6, -0.6]

      n = 1
      time = take 7 [0 ..]

      pMap =  M.fromList [ (PPosIdx 0 1, mkSig n s01 .++ (sfromList [head s01] :: PSigL)),
                           (PPosIdx 1 0, mkSig n s10 .++ (sfromList [head s10] :: PSigL)), 
                           (PPosIdx 1 2, mkSig n s12 .++ (sfromList [head s12] :: PSigL)),
                           (PPosIdx 2 1, mkSig n s21 .++ (sfromList [head s21] :: PSigL)),
                           (PPosIdx 1 3, mkSig n s13 .++ (sfromList [head s13] :: PSigL)),
                           (PPosIdx 3 1, mkSig n s31 .++ (sfromList [head s31] :: PSigL)) ]

      pRec = PowerRecord (sfromList time) pMap
      (_, sqTopo) = makeSequence pRec topo

      powers = [1 .. 10] :: [Double]
      etas = [0.1 .. 10]:: [Double]
      
      m = genVariationMatrix powers etas

      res = map (map f) m -- (sequence [lst, etas])
      f (x,y) = (x,y,g x y (x+y))
{-      
      f (x,y) = (x, y, g (head $ stoList $ ((energyMap $ variation sqTopo x y) M.! (EnergyIdx 0 0 0 1)))
                            (head $ stoList $ ((energyMap $ variation sqTopo x y) M.! (EnergyIdx 0 0 2 1)))
                            (head $ stoList $ ((energyMap $ variation sqTopo x y) M.! (EnergyIdx 1 0 6 5))) )
-}
      g e0001 e0021 e1021 = (e0021 + e1021)/e0001
  
  
  let plotAttrs name = [Title ("Optimisation Result: " ++ name), 
                    Grid $ Just [], 
                    XLabel ("Power [" ++ (show $ getDisplayUnit Typ_P) ++ "]"),
                    YLabel ("Efficiency [" ++ (show $ getDisplayUnit Typ_N) ++ "]"), 
                    Size $ Scale 0.7]

  
  report [] ("Res",res)
  plotMesh3d (plotAttrs "Surface") [Plot3dType Surface] res
  plotMesh3d (plotAttrs "ColorMap") [Plot3dType ColorMap] res


