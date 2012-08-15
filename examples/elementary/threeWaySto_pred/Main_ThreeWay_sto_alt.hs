{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators  #-}

module Main where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import System.Process
import System.Exit

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
solve3Way sqTopo y n = interpretFromScratch (SingleRecord 0) 1 gd -- interprete and solve equations

{-
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
-}
                    
  -- Sequence 0 Primary Source Active additionally charging storage
  where givenEnv0 = emptyEnv { recordNumber = SingleRecord 0,
                               dtimeMap = M.fromList [ (DTimeIdx 0 0, sfromList [(1-y)*dt]) ],
                               powerMap = M.fromList [ (PowerIdx 0 0 2 1, sfromList [pCons])],
                               energyMap = M.fromList [ (EnergyIdx 0 0 3 1, sfromList [(dt*y*pCons/n/0.9)])],
                               fetaMap =  M.fromList [ (FEtaIdx  0 0 0 1, smap etaf), (FEtaIdx 0 0 1 0, undefined),
                                                       (FEtaIdx  0 0 1 2, smap (const 0.9)), (FEtaIdx 0 0 2 1, smap (const 0.9)),
                                                       (FEtaIdx  0 0 1 3, smap (const n)), (FEtaIdx 0 0 3 1, smap (const n)) ] }
        -- Sequence 1 -- using storage only
        givenEnv1 = emptyEnv { recordNumber = SingleRecord 0,
                               dtimeMap = M.fromList [ (DTimeIdx 1 0, sfromList [y*dt])],
                               energyMap = M.fromList [ ],
                               powerMap =  M.fromList [ (PowerIdx 1 0 2 1, sfromList [pCons])],
                               fetaMap =   M.fromList [ (FEtaIdx 1 0 0 1, smap etaf), (FEtaIdx 1 0 1 0, undefined),
                                                        (FEtaIdx 1 0 1 2, smap (const 0.9)), (FEtaIdx 1 0 2 1, smap (const 0.9)),
                                                        (FEtaIdx 1 0 1 3, smap (const n)), (FEtaIdx 1 0 3 1, smap (const n)) ] }
                    
         -- Variable Efficiency function at Source (backwards lookup)            
--        etaf x = 1/((x+sqrt(x*x+4*x))/(2*x))
        etaf x = 1/((x+sqrt(x*x+4*x))/(2*x))
        
        pCons = 1.0
        dt = 1
        


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

clearCurves ::  IO ExitCode
clearCurves = do
  system ("rm curve.gp")
  system ("rm *.csv")
  

-- | 
saveCurves :: String -> IO ExitCode
saveCurves dirName = do   
  system ("mv curve.gp " ++ dirName)
  system ("mv *.csv " ++ dirName)

-- | Main Function ================================================================== 

main :: IO ()
main = do
  let 

    -- | A. -- Define Grid and solve system ------------------------------------    
    
    -- define Variation
    yIndir = [0,0.2 .. 0.9] :: [Val]
    etas = [0.6,0.7 .. 1] :: [Val]

    -- generate Variation Grid
    (varY,varN) = genVariationMatrix yIndir etas
    
    -- solve System over Variation Grid
    varEnvs = zipWith (zipWith (solve3Way sqTopo)) varY varN :: [[Envs UTFSig]]
    
    -- | B. -- Extract Values for further calulations
    
    -- result of energies out 
    storagePerc = sfromList2 varY :: Test2 (Typ A Y Tt) Val 
    storageEfficiency = sfromList2 varN :: Test2 (Typ A N Tt) Val 
    
    -- consumer
    powerConsumptionS0 = getVarPower varEnvs (PowerIdx 0 0 2 1)
    powerConsumptionS1 = getVarPower varEnvs (PowerIdx 1 0 6 5)
    energyConsumption = (getVarEnergy varEnvs  (EnergyIdx 0 0 2 1)) .+  makeDelta (getVarEnergy varEnvs  (EnergyIdx 1 0 6 5)) 
    
    -- internal power (Between System 1 and 2)
    powerInt = getVarPower varEnvs (PowerIdx 0 0 1 0)
    energyInt = (getVarEnergy varEnvs (EnergyIdx 0 0 1 0)) -- .+  (makeDelta $ getVarEnergy varEnvs (EnergyIdx 1 0 1 0))
    
    -- energy source
    powerSource = getVarPower varEnvs (PowerIdx 0 0 0 1)
    energySource = getVarEnergy varEnvs  (EnergyIdx 0 0 0 1) 
    
    -- | C. -- Calculate Additional Values
    
    -- system efficiencie
    etaSYS =  energyConsumption./energySource 
    etaSYS1 =  energyInt./energySource 
    etaSYS2 =  energyConsumption./energyInt 
    
    -- eta1 efficiency check
    pOne = toScalar 1 :: TC Scalar (Typ A P Tt) (Data Nil Val) 
    etaOne =  toScalar 1 :: TC Scalar (Typ A N Tt) (Data Nil Val) 
    etaSYS1_Check = makeAbsolut $ etaOne .- (pOne./ ((makeDelta pOne) .+powerSource)) ::  Test2 (Typ A N Tt) Val
    check = etaSYS1_Check .- etaSYS1
      
    -- calculating system losses
    lossSYS = energySource .- energyConsumption 
    lossSYS1 = energySource .- energyInt 
    lossSYS2 = energyInt .- energyConsumption 
        
    
  -- | D. -- Selected Energy Flow Plots
 
--  drawTopologyX' sqTopo
  drawTopology  sqTopo (head(head(varEnvs))) 
  drawTopology  sqTopo (last(head(varEnvs))) 
  drawTopology  sqTopo (head(last(varEnvs))) 
  drawTopology  sqTopo (last(last(varEnvs))) 
  
  -- | E. -- Reporting the Numbers
  
  putStrLn "Efficiency Eta1 and Check"
  report [] ("etaSYS1", etaSYS1)
  report [] ("etaSYS1_check", etaSYS1_Check)
  report [] ("Check", check)  

  putStrLn "Consumer"
  report [] ("energyConsumption",energyConsumption)  
  report [] ("energyInt",energyInt)  
  report [] ("energySource",energySource)  
  
  putStrLn "Consumer"
  report [] ("powerInt", powerInt)
  report [] ("powerSource", powerSource)
  
  report [] ("System Efficiency",  etaSYS)
  report [] ("System1 Efficiency", etaSYS1)
  report [] ("System2 Efficiency", etaSYS2)
    
  -- | F. -- Surface & Trend Plots
    
  -- additional dataFormat for trend plots
  let lossListSYS1 = toSigList lossSYS1
      lossListSYS2 = toSigList lossSYS2
      lossListSYS = toSigList lossSYS
--  sigPlots [head lossListSYS1,  head lossListSYS2]
--  sigPlots [last lossListSYS1,  last lossListSYS2]
      
  
  let etaListSYS = toSigList etaSYS
      etaListSYS1 = toSigList etaSYS1
      powerListSource = toSigList powerSource
      powerListInt = toSigList powerInt
  
  xyplot "Efficiency System1" (sfromList yIndir :: Test1 (Typ A Y Tt) Val) (etaSYS1)
  xyplot "InternalPower" (sfromList yIndir :: Test1 (Typ A Y Tt) Val) (powerInt)
  
  xyplot "SystemLoss" (sfromList yIndir :: Test1 (Typ A Y Tt) Val) (lossSYS)
      
  -- xyplots "SystemLoss1 & SystemLoss2"(sfromList yIndir :: Test1 (Typ A Y Tt) Val) (lossListSYS1 ++ (map sreverse lossListSYS2))
  xyplot "Loss System1 & Loss System2"(sfromList yIndir :: Test1 (Typ A Y Tt) Val) (lossSYS1 .++ lossSYS2)
       
--  surfPlot "System Loss" storagePerc storageEfficiency lossSYS 
  surfPlot "System1 Loss" storagePerc storageEfficiency  lossSYS1 
  
  surfPlot "System2 Loss" storagePerc storageEfficiency  lossSYS2 
  
  xyplot "System Efficiency" (sfromList yIndir :: Test1 (Typ A Y Tt) Val) etaListSYS
--  xyplots2 powerListInt etaListSYS
  

  surfPlot "System Efficiency" storagePerc storageEfficiency etaSYS
  
  surfPlot "System1 Efficiency" storagePerc storageEfficiency etaSYS1
  
  surfPlot "System2 Efficiency" storagePerc storageEfficiency etaSYS2
  
  surfPlot "Flow Share over Storage" storagePerc storageEfficiency storagePerc

  surfPlot "Energy Consumption" storagePerc storageEfficiency energyConsumption

--  surfPlot "powerConsumptionS1" storagePerc storageEfficiency powerConsumptionS1
--  surfPlot "powerConsumptionS0" storagePerc storageEfficiency powerConsumptionS0  
    


