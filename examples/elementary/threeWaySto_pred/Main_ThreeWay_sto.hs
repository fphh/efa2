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

import EFA2.Signal.Signal
import EFA2.Signal.Data
import EFA2.Signal.Typ

import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData
import EFA2.Display.ReportSequence
import EFA2.Display.Plot
import EFA2.Utils.Utils


topo :: Topology
topo = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, Source), (1, Crossing), (2, Sink), (3, Storage 0)]
        edges = [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel)]

mkSig :: Int -> ([Val] -> PSig)
mkSig n = sfromList . concat . replicate n

--etaf x = x/(x+5)
--revetaf x = (x+5)/x

etaf x = sqrt x
revetaf x = 1/(sqrt x)

--revFunc :: (a -> a) (a -> a)
--revFunc f x = 1/(f x)


--variation :: Topology -> Val -> [Envs UTFSig]
variation sqTopo x y = interpretFromScratch 1 gd
  where 
        givenEnv0 = emptyEnv { dtimeMap = M.fromList [ (DTimeIdx 0 0, sfromList [1.0]) ],
                               powerMap = M.fromList [ (PowerIdx 0 0 3 1, sfromList [x]),
                                                       (PowerIdx 0 0 2 1, sfromList [0.6] :: UTFSig) ],
                               fetaMap = M.fromList [ (FEtaIdx 0 0 0 1, smap etaf), (FEtaIdx 0 0 1 0, smap revetaf),
                                                      (FEtaIdx 0 0 1 2, smap (const 0.7)), (FEtaIdx 0 0 2 1, smap (const 1.7)),
                                                      (FEtaIdx 0 0 1 3, smap (const y)), (FEtaIdx 0 0 3 1, smap (const (1/y))) ] }

        givenEnv1 = emptyEnv { --dtimeMap = M.fromList [ (DTimeIdx 1 0, sfromList [1.0]) ],
                               energyMap = M.fromList [ (EnergyIdx 1 0 3 1, sfromList [x]) ],
                               powerMap = M.fromList [ (PowerIdx 1 0 2 1, sfromList [0.6]) ],
                               fetaMap = M.fromList [ (FEtaIdx 1 0 0 1, smap etaf), (FEtaIdx 1 0 1 0, smap revetaf),
                                                      (FEtaIdx 1 0 1 2, smap (const 0.7)), (FEtaIdx 1 0 2 1, smap (const 1.7)),
                                                      (FEtaIdx 1 0 1 3, smap (const (1/y))), (FEtaIdx 1 0 3 1, smap (const y)) ] }
{-
        givenEnv2 = emptyEnv { dtimeMap = M.fromList [ (DTimeIdx 2 0, sfromList [1.0]) ],
                               powerMap = M.fromList [ (PowerIdx 2 0 3 1, sfromList [x]), (PowerIdx 2 0 2 1, sfromList [0.6]) ],
                               fetaMap = M.fromList [ (FEtaIdx 2 0 0 1, smap etaf), (FEtaIdx 2 0 1 0, smap revetaf),
                                                      (FEtaIdx 2 0 1 2, smap (const 0.7)), (FEtaIdx 2 0 2 1, smap (const 1.7)),
                                                      (FEtaIdx 2 0 1 3, smap (const 0.7)), (FEtaIdx 2 0 3 1, smap (const 1.7)) ] }

        givenEnv3 = emptyEnv { --dtimeMap = M.fromList [ (DTimeIdx 3 0, sfromList [1.0]) ],
                               energyMap = M.fromList [ (EnergyIdx 1 0 3 1, sfromList [x]) ],
                               powerMap = M.fromList [ (PowerIdx 1 0 2 1, sfromList [0.6]) ],
                               fetaMap = M.fromList [ (FEtaIdx 3 0 0 1, smap etaf), (FEtaIdx 3 0 1 0, smap revetaf),
                                                      (FEtaIdx 3 0 1 2, smap (const 0.7)), (FEtaIdx 3 0 2 1, smap (const 1.7)),
                                                      (FEtaIdx 3 0 1 3, smap (const 1.7)), (FEtaIdx 3 0 3 1, smap (const 0.7)) ] }

-}
      
        (sqEnvs, ts') = makeAllEquations sqTopo [givenEnv0, givenEnv1 ] -- , givenEnv2, givenEnv3 ]


        storage0 = EnergyIdx (-1) 0 8 9
        dtime0 = DTimeIdx (-1) 0
        ts = [give storage0, give dtime0] ++ ts'

        sqEnvs' = sqEnvs { dtimeMap = M.insert (DTimeIdx (-1) 0) (sfromList [1.0]) (dtimeMap sqEnvs),
                           energyMap = M.insert storage0 (sfromList [3.0]) (energyMap sqEnvs) }

        gd = map (eqToInTerm sqEnvs') (order ts)


getEnergy :: EnergyIdx -> [Envs UTFSig] -> Test1 (Typ A F Tt) Val
getEnergy idx envs = setTypeTestRow $ setType $ mconcat $ map (flip safeLookup idx) pm
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

      n = 1
      --l = fromIntegral $ length $ replicate n (s01 ++ s01')
      --time = [0, 0] ++ (concatMap (replicate 3) [1.0 .. l])
      time = take 7 [0 ..]

      pMap =  M.fromList [ (PPosIdx 0 1, mkSig n s01 .++ (sfromList [head s01] :: PSig)),
                           (PPosIdx 1 0, mkSig n s10 .++ (sfromList [head s10] :: PSig)), 
                           (PPosIdx 1 2, mkSig n s12 .++ (sfromList [head s12] :: PSig)),
                           (PPosIdx 2 1, mkSig n s21 .++ (sfromList [head s21] :: PSig)),
                           (PPosIdx 1 3, mkSig n s13 .++ (sfromList [head s13] :: PSig)),
                           (PPosIdx 3 1, mkSig n s31 .++ (sfromList [head s31] :: PSig)) ]

      pRec = PowerRecord (sfromList time) pMap
      (_, sqTopo) = makeSequence pRec topo

      lst = [0, 0.5 .. 5.0]
      etas = [ 0.7, 0.72 .. 0.9 ]
      --res :: [[Envs UTFSig]]
      --res :: [[(Val, Val, Val)]]
      res = map f (sequence [lst, etas])
      f (x:y:_) = [(x, y, g (head $ stoList $ ((energyMap $ variation sqTopo x y) M.! (EnergyIdx 0 0 0 1)))
                            (head $ stoList $ ((energyMap $ variation sqTopo x y) M.! (EnergyIdx 0 0 2 1)))
                            (head $ stoList $ ((energyMap $ variation sqTopo x y) M.! (EnergyIdx 1 0 6 5))) )]
      g e0001 e0021 e1021 = (e0021 + e1021)/e0001

{-
      cwith f x y = map g x
        where g xi = map (f xi) y
      --res2 :: [Test1 ..]
      --res2 = fconv $ map (getEnergy (EnergyIdx 0 0 0 1)) res

      s :: [(Val, Val)]
      s = map (\(x:y:_) -> (x, y)) $ sequence [lst, etas]

      res2 :: [[Val]]
      res2 = map (stoList . getEnergy (EnergyIdx 0 0 0 1)) res
-}

{-
      pdt = map dtimeMap res
      dt = map (head . stoList . (M.! DTimeIdx 1 0)) pdt

      metas = map fetaMap res
      etas = map (M.! FEtaIdx 0 0 0 1) metas

      ems = map energyMap res
      e0001' = map (M.! EnergyIdx 0 0 0 1) ems
      e0001 = map (head . stoList) e0001'

      ns = zipWith f lst $ map (head . stoList) (zipWith ($) etas e0001')

      dts = zipWith f lst dt
      f x y = show x ++ " " ++ show y

      e0001s = zipWith f lst e0001

      e0021 = map (head . stoList . (M.! EnergyIdx 0 0 2 1)) ems
      e1021 = map (head . stoList . (M.! EnergyIdx 1 0 6 5)) ems

      nsys = zipWith f lst (zipWith3 g e0001 e0021 e1021)
      g e0001 e0021 e1021 = (e0021 + e1021)/e0001
-}
      --energies = getEnergy (EnergyIdx 0 0 0 1) res

  --sigPlot res2
  --print (head res)
  --mapM_ (drawTopology sqTopo) res

  --putStrLn (L.intercalate "\n" dts)
  --putStrLn (L.intercalate "\n" e01s)
  --putStrLn (L.intercalate "\n" ns)
  --putStrLn (L.intercalate "\n" nsys)
  --print e1021

  --print (head ems)
  --print (zipWith (\x y -> y - x) pws (tail pws))
  plotMesh3d [] [] res

  print "That's it!"
