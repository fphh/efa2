{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators  #-}

module Main where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

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

import EFA2.Display.Report
import EFA2.Display.DispSignal
import EFA2.Display.DrawGraph


-- | A. Generate System Topology definition
topo :: Topology
topo = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, Source), (1, Crossing), (2, Sink), (3, Storage 0)]
        edges = [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel)]
        
-- | B. Generating a sequence Topology using a fake dataset
sqTopo :: Topology
sqTopo = sqTopo
  where
      s01 = [0, 2, 2, 0, 0, 0]
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

      mkSig :: Int -> ([Val] -> PSigL)
      mkSig n = sfromList . concat . replicate n

-- | C. System solving
solve3Way :: Topology -> Val -> Val -> Envs UTFSig
solve3Way sqTopo x y = interpretFromScratch (SingleRecord 0) 1 gd -- interprete and solve equations

  -- Sequence 0 Primary Source Active additionally charging storage
  where givenEnv0 = emptyEnv { recordNumber = SingleRecord 0,
                               dtimeMap = M.fromList [ (DTimeIdx 0 0, sfromList [1.0]) ],
                               powerMap = M.fromList [ (PowerIdx 0 0 3 1, sfromList [x]),
                                                       (PowerIdx 0 0 2 1, sfromList [0.6])],
                               fetaMap =  M.fromList [ (FEtaIdx  0 0 0 1, smap etaf), (FEtaIdx 0 0 1 0, undefined),
                                                       (FEtaIdx  0 0 1 2, smap (const 1)), (FEtaIdx 0 0 2 1, smap (const 1)),
                                                       (FEtaIdx  0 0 1 3, smap (const y)), (FEtaIdx 0 0 3 1, smap (const y)) ] }
        -- Sequence 1 -- using storage only
        givenEnv1 = emptyEnv { recordNumber = SingleRecord 0,
                           --    dtimeMap = M.fromList [ (DTimeIdx 1 0, sfromList [1.0]) ],
                               energyMap = M.fromList [ (EnergyIdx 1 0 3 1, sfromList [x] :: UTFSig) ],
                               powerMap =  M.fromList [ (PowerIdx 1 0 2 1, sfromList [0.6]) ],
                               fetaMap =   M.fromList [ (FEtaIdx 1 0 0 1, smap etaf), (FEtaIdx 1 0 1 0, undefined),
                                                        (FEtaIdx 1 0 1 2, smap (const 1)), (FEtaIdx 1 0 2 1, smap (const 1)),
                                                        (FEtaIdx 1 0 1 3, smap (const y)), (FEtaIdx 1 0 3 1, smap (const y)) ] }
                    
        -- Variable Efficiency function at Source (backwards lookup)            
        etaf x = 1/((x+sqrt(x*x+4*x))/(2*x))

        -- Generate Equations for both Sections
        (sqEnvs, ts') = makeAllEquations sqTopo [givenEnv0, givenEnv1]

        -- set initial values in equation system
        storage0 = EnergyIdx (-1) 0 8 9
        dtime0 = DTimeIdx (-1) 0
        ts = [give storage0, give dtime0] ++ ts'
        sqEnvs' = sqEnvs { dtimeMap = M.insert (DTimeIdx (-1) 0) (sfromList [1.0]) (dtimeMap sqEnvs),
                           energyMap = M.insert storage0 (sfromList [3.0]) (energyMap sqEnvs) }
                  
        -- rearrange equations       
        gd = map (eqToInTerm sqEnvs') (toAbsEqTermEquations $ order ts)
        

{-
-- | D. Function to generate Variation Matrix
genVariationMatrix :: (UV.Unbox d2, UV.Unbox d1) => Test1 t1 d1 -> Test1 t2 d2 ->  (Test2 t1 d1, Test2 t2 d2)
genVariationMatrix xs ys = (fromSigList $ replicate (slength ys) xs,stranspose $ fromSigList $ replicate (slength xs) ys)
-}

-- | D. Function to generate Variation Matrix
genVariationMatrix :: [a] -> [b] -> ([[a]], [[b]])
genVariationMatrix xs ys = (replicate (length ys) xs, L.transpose $ replicate (length xs) ys)


-- | Safe Lookup Functions
getVarEnergy :: [[Envs UTFSig]] ->  EnergyIdx -> Test2 (Typ A F Tt) Val 
getVarEnergy varEnvs idx = changeSignalType $ sfromCells $ map (map f ) varEnvs  
  where f ::  Envs UTFSig ->   FSamp
        f envs = changeType $ shead $ ((safeLookup (energyMap envs) idx)) 

-- | Safe Lookup Functions
getVarPower :: [[Envs UTFSig]] ->  PowerIdx -> Test2 (Typ A P Tt) Val 
getVarPower varEnvs idx = changeSignalType $ sfromCells $ map (map f ) varEnvs  
  where f ::  Envs UTFSig ->   PFSamp
        f envs = changeType $ shead $ ((safeLookup (powerMap envs) idx)) 

{-
-- | Safe Lookup Functions
getPower :: Envs UTFSig ->  PowerIdx -> PFSamp
getPower envs idx = changeType $ shead $ safeLookup (powerMap envs) idx 
-}
{-
-- | Safe Lookup Functions
getEff :: (Envs UTFSig) ->  EtaIdx -> NSig 
getEff envs idx = safeLookUp envs idx 
-}



main :: IO ()
main = do
  let 
    -- define Variation
    powers = [0.1,0.2 .. 0.7] :: [Val]
    etas = [0.9,0.91 .. 1] :: [Val]

    -- generate Variation Grid
    (varP,varN) = genVariationMatrix powers etas
    
    -- solve System over Variation Grid
    varEnvs = zipWith (zipWith (solve3Way sqTopo)) varP varN :: [[Envs UTFSig]]
    
    -- Result of Energies out 
    storagePower = sfromList2 varP :: Test2 (Typ A P Tt) Val 
    storageEfficiency = sfromList2 varN :: Test2 (Typ A N Tt) Val 
    
    powerConsumptionS0 = getVarPower varEnvs (PowerIdx 0 0 2 1)
    powerConsumptionS1 = getVarPower varEnvs (PowerIdx 1 0 6 5)
    
    powerInt = getVarPower varEnvs (PowerIdx 0 0 1 0)
    energyInt = (getVarEnergy varEnvs (EnergyIdx 0 0 1 0)) -- .+  (makeDelta $ getVarEnergy varEnvs (EnergyIdx 1 0 1 0))
    
    
    energySource = getVarEnergy varEnvs  (EnergyIdx 0 0 0 1) 
    energyConsumption = (getVarEnergy varEnvs  (EnergyIdx 0 0 2 1)) .+  makeDelta (getVarEnergy varEnvs  (EnergyIdx 1 0 6 5)) 
    
    etaSYS =  energyConsumption./energySource 
    etaSYS1 =  energyInt./energySource 
    etaSYS2 =  energySource./energyInt 
    
    lossSYS = energySource .- energyConsumption 
    lossSYS1 = energySource .- energyInt 
    lossSYS2 = energyInt .- energyConsumption 
        
 
--  drawTopologyX' sqTopo
  drawTopology  sqTopo (head(head(varEnvs))) 
  drawTopology  sqTopo (last(head(varEnvs))) 
  drawTopology  sqTopo (head(last(varEnvs))) 
  drawTopology  sqTopo (last(last(varEnvs))) 
  
  
--  report [] ("powerConsumptionS0",powerConsumptionS0)  
--  report [] ("powerConsumptionS1",powerConsumptionS1)  
  report [] ("energyConsumption",energyConsumption)  
  report [] ("energyInt",energyInt)  
  report [] ("energySource",energySource)  
  
  report [] ("System Efficiency",  etaSYS)
  report [] ("System1 Efficiency", etaSYS1)
  report [] ("System2 Efficiency", etaSYS2)
  
  
--  surfPlot "powerConsumptionS0" storagePower storageEfficiency powerConsumptionS0
--  surfPlot "powerConsumptionS1" storagePower storageEfficiency powerConsumptionS1
  
--  surfPlot "energyConsumption" storagePower storageEfficiency energyConsumption
    
  surfPlot "System Efficiency" storagePower storageEfficiency etaSYS
  surfPlot "System1 Efficiency" storagePower storageEfficiency etaSYS1
  surfPlot "System2 Efficiency" storagePower storageEfficiency etaSYS2

  surfPlot "System Loss" storagePower storageEfficiency lossSYS 
  surfPlot "System1 Loss" storagePower storageEfficiency  lossSYS1 
  surfPlot "System2 Loss" storagePower storageEfficiency  lossSYS2 
  
  let lossListSYS1 = toSigList lossSYS1
      lossListSYS2 = toSigList lossSYS2
  sigPlots [head lossListSYS1,  head lossListSYS2]
  sigPlots [last lossListSYS1,  last lossListSYS2]
