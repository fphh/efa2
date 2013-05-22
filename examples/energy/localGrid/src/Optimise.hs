{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where


--import qualified Modules.Signals as Signals
import qualified Modules.System as System
import Modules.System (Node(..))
--import qualified Modules.Analysis as Analysis
import qualified Modules.Optimisation as Optimisation
import Modules.Optimisation (sec0,sec1)
import Modules.Utility as ModUt

import EFA.Report.Report(report)

--import qualified EFA.Signal.Record as Record
import EFA.Utility.Async (concurrentlyMany_)
import qualified EFA.Graph.Draw as Draw
--import EFA.IO.PLTImport (modelicaPLTImport)
import qualified EFA.Signal.Signal as Sig
--import qualified EFA.Signal.Vector as V
--import EFA.Signal.Data (Data(..), Nil, (:>))
--import EFA.Signal.Typ (Typ, F, T, A, Tt)
--import qualified EFA.Signal.SequenceData as SD
--import qualified EFA.Utility as Utility
--import EFA.Signal.Sequence (makeSeqFlowTopology)
--import qualified EFA.Graph.Flow as Flow
--import qualified EFA.Graph.Draw as Draw
--import EFA.Graph(lefilter)
--import qualified EFA.Signal.Plot as Plot
--import EFA.Graph.Topology(isStructureEdge)
import qualified EFA.Example.Index as XIdx
--import qualified EFA.Equation.Arithmetic as Arith
--import qualified EFA.Equation.Environment as Env
-- import qualified EFA.Signal.ConvertTable as Table
import qualified EFA.Example.Absolute as EqGen
--import qualified EFA.Example.Index as XIdx
--import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.Record as EqRec
import EFA.Equation.Result (Result(..))
--import EFA.Utility.Map (checkedLookup)
import qualified EFA.Signal.PlotIO as PlotIO

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Data.GraphViz.Attributes.Colors.X11 as Colors

import qualified EFA.Signal.ConvertTable as CT
import qualified EFA.IO.TableParser as Table

import qualified System.IO as IO
--import System.Environment (getEnv)
--import System.FilePath ((</>))

--import qualified EFA.Utility.Stream as Stream

-- import Data.Monoid (mconcat, (<>))
--import EFA.Example.Absolute ( (.=), (%=), (=.=) )
--import EFA.Utility.Stream (Stream((:~)))

--import qualified Data.List as L
import qualified Data.Map as M
--import qualified Modules.Analysis as Analysis
import Data.Tuple.HT (mapPair)

import qualified Data.Maybe as Maybe


plotTerm :: DefaultTerm.T
plotTerm = DefaultTerm.cons

colours :: [Colors.X11Color]
colours = [ Colors.White,	
            Colors.Gray90,	
            Colors.Gray80,	
            Colors.Gray70 ]
          
-- ################### Vary Operation Demand

restPower :: [Double]
restPower = [0,0.2 .. 0.8]

localPower :: [Double]
localPower = [0.2,0.4 .. 1.0]

varRestPower', varLocalPower' :: [[Double]]
(varRestPower', varLocalPower') = CT.varMat restPower localPower

restPowerScale :: Double
restPowerScale = 1

localPowerScale :: Double
localPowerScale = 1

varRestPower :: Sig.PTestRow2 [] [] Double
varRestPower = Sig.fromList2 varRestPower'

varLocalPower :: Sig.PTestRow2 [] [] Double
varLocalPower = Sig.fromList2 varLocalPower'


-- ################### Vary Degrees of Freedom

waterPower :: [Double]
waterPower = [0,0.2 .. 0.8]

gasPower :: [Double]
gasPower = [0, 0.2 .. 1.0]

powerScaleWater ::  Double
powerScaleWater = 1
  
powerScaleGas ::  Double
powerScaleGas = 1

powerScaleTransformer ::  Double
powerScaleTransformer = 1

powerScaleCoal ::  Double
powerScaleCoal = 5  
  
varWaterPowerSig :: Sig.PTestRow [] Double
varWaterPowerSig = Sig.fromList waterPower

varGasPowerSig :: Sig.PTestRow [] Double
varGasPowerSig = Sig.fromList gasPower

varWaterPower', varGasPower' :: [[Double]]
(varWaterPower', varGasPower') = CT.varMat waterPower gasPower

varWaterPower :: Sig.PSignal2 [] [] Double
varWaterPower = Sig.fromList2 varWaterPower'

varGasPower :: Sig.PSignal2 [] [] Double
varGasPower = Sig.fromList2 varGasPower'

noLegend :: Int -> String
noLegend =  (const "")

legend :: Int -> String
legend 0 = "Laden"
legend 1 = "Entladen"
legend _ = "Undefined"

           -- | Sweep DOF (Degree of Fredom) Space
sweepCharge :: (Double -> Double) ->
               (Double -> Double) ->
               (Double -> Double) ->
               (Double -> Double) ->
               Double -> 
                Double -> 
                [[Double]] -> 
                [[Double]] -> 
                Sig.UTSignal2 [] [] 
                (EqEnv.Complete  
                 Node
                 (EqRec.Absolute (Result Double))
                 (EqRec.Absolute (Result Double)))

sweepCharge etaWaterCharge etaCoal etaGas etaTransHL rPower lPower wPower gPower = Sig.fromList2 $ 
                                          zipWith (zipWith (Optimisation.solveCharge 
                                                            etaWaterCharge etaCoal etaGas 
                                                            etaTransHL rPower lPower)) 
                                          wPower gPower
     
    -- Sweep all Configuration Optimations
sweepDischarge :: (Double -> Double) ->
                  (Double -> Double) ->
                  (Double -> Double) ->
                  (Double -> Double) ->
                  Double -> 
                  Double -> 
                  [[Double]] -> 
                  [[Double]] -> 
                  Sig.UTSignal2 [] []
                  ((EqEnv.Complete  
                    Node
                    (EqRec.Absolute (Result Double))
                    (EqRec.Absolute (Result Double))))
sweepDischarge etaWaterDischarge etaCoal etaGas etaTransHL rPower lPower wPower gPower = Sig.fromList2 $ 
                                              zipWith (zipWith (Optimisation.solveDischarge 
                                                                etaWaterDischarge etaCoal etaGas 
                                                                etaTransHL rPower lPower)) 
                                              wPower gPower
calcEtaSys :: (EqEnv.Complete  
               Node
               (EqRec.Absolute (Result Double))
               (EqRec.Absolute (Result Double)))
              -> Double
calcEtaSys env =  if etaSys < 1 then etaSys else -10^12
     where
     eGas0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Gas LocalNetwork)) env
     eCoal0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Coal Network)) env
     eRest0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Rest Network)) env
     eRestLocal0 = (ModUt.lookupAbsEnergy (XIdx.energy sec0 LocalRest LocalNetwork)) env
     eGas1 = (ModUt.lookupAbsEnergy (XIdx.energy sec1 Gas LocalNetwork)) env
     eCoal1 =  (ModUt.lookupAbsEnergy (XIdx.energy sec1 Coal Network)) env
     eRest1 =  (lookupAbsEnergy (XIdx.energy sec1 Rest Network)) env
     eRestLocal1 =  (lookupAbsEnergy (XIdx.energy sec1 LocalRest LocalNetwork)) env
     etaSys = (eRest0 + eRest1 + eRestLocal0 + eRestLocal1) / (eGas0  + eGas1 + eCoal0 + eCoal1)
                 

maxEta :: Sig.UTSignal2 [] [] 
         (EqEnv.Complete  
         Node
         (EqRec.Absolute (Result Double))
         (EqRec.Absolute (Result Double))) ->
         (Double,(Maybe Sig.SignalIdx, Maybe Sig.SignalIdx))
maxEta sigEnvs = (Sig.fromScalar maxEta, (xIdx, yIdx)) 
  where 
    etaSys = Sig.map calcEtaSys sigEnvs
    maxEta = Sig.maximum etaSys
    (xIdx, yIdx) = Sig.findIndex2 (== Sig.fromScalar maxEta) etaSys
    

-- Sig.fromScalar $

main :: IO ()
main = do

   IO.hSetEncoding IO.stdout IO.utf8
    
   tabEta <- Table.read "../simulation/maps/eta.txt"

   let etaWaterCharge :: Double -> Double
       etaWaterCharge = Sig.fromSample . Sig.interp1Lin "etaWaterCharge" (Sig.scale xs powerScaleWater) (head ys) . Sig.toSample
         where xs :: Sig.PSignal [] Double
               ys :: [Sig.NSignal [] Double]
               (xs,ys) = CT.convertToSignal2D (M.lookup "storage" tabEta)
                        
   let etaWaterDischarge :: Double -> Double
       etaWaterDischarge = Sig.fromSample . Sig.interp1Lin "etaWaterDischarge" (Sig.scale xs powerScaleWater) (head ys) . Sig.toSample
         where xs :: Sig.PSignal [] Double
               ys :: [Sig.NSignal [] Double]
               (xs,ys) = CT.convertToSignal2D (M.lookup "storage" tabEta)
   
   let etaGas :: Double -> Double
       etaGas = Sig.fromSample . Sig.interp1Lin "etaGas" (Sig.scale xs powerScaleGas) (head ys) . Sig.toSample
         where xs :: Sig.PSignal [] Double
               ys :: [Sig.NSignal [] Double]
               (xs,ys) = CT.convertToSignal2D (M.lookup "gas" tabEta)
               
   let etaCoal :: Double -> Double
       etaCoal = Sig.fromSample . Sig.interp1Lin "etaCoal" (Sig.scale xs powerScaleCoal) (head ys) . Sig.toSample
         where xs :: Sig.PSignal [] Double
               ys :: [Sig.NSignal [] Double]
               (xs,ys) = CT.convertToSignal2D (M.lookup "coal" tabEta)
   
   let etaTransHL :: Double -> Double
       etaTransHL = Sig.fromSample . Sig.interp1Lin "etaTransHL" (Sig.scale xs powerScaleTransformer) (head ys) . Sig.toSample
         where xs :: Sig.PSignal [] Double
               ys :: [Sig.NSignal [] Double]
               (xs,ys) = CT.convertToSignal2D (M.lookup "transformer" tabEta)

   let 
     

     
    -- | Map Operation space room
     envsCharge :: Sig.UTTestRow2 [] [] (Sig.UTSignal2 [] []
                         (EqEnv.Complete  
                           Node
                           (EqRec.Absolute (Result  Double))
                           (EqRec.Absolute (Result  Double))))
     envsCharge = Sig.fromList2 $ zipWith (zipWith (\ x y -> sweepCharge etaWaterCharge etaCoal etaGas etaTransHL x y varWaterPower' varGasPower')) varRestPower' varLocalPower'


     envsDischarge :: Sig.UTTestRow2 [] [] (Sig.UTSignal2 [] []
                           (EqEnv.Complete  
                             Node
                             (EqRec.Absolute (Result Double))
                             (EqRec.Absolute (Result Double))))
     envsDischarge = Sig.fromList2 $ zipWith (zipWith (\ x y -> sweepDischarge etaWaterCharge etaCoal etaGas etaTransHL x y varWaterPower' varGasPower')) varRestPower' varLocalPower'

     maxETACharge :: Sig.NTestRow2 [] [] Double
     maxETACharge = Sig.setType $ Sig.map fst $ Sig.map maxEta envsCharge
     maxETADischarge :: Sig.NTestRow2 [] [] Double
     maxETADischarge = Sig.setType $ Sig.map fst $ Sig.map maxEta envsDischarge

     indexCharge :: Sig.UTTestRow2 [] [] (Maybe Sig.SignalIdx,Maybe Sig.SignalIdx) 
     indexCharge   = Sig.map snd $ Sig.map maxEta envsCharge
     
     indexCharge' :: Sig.UTTestRow2 [] [] (Double,Double)
     indexCharge'  = Sig.map ( mapPair (fromIntegral . Sig.unSignalIdx . Maybe.fromJust, fromIntegral . Sig.unSignalIdx . Maybe.fromJust)) $ a
                   where 
                     a :: Sig.UTTestRow2 [] [] (Maybe Sig.SignalIdx,Maybe Sig.SignalIdx)
                     a = Sig.map snd $ Sig.map maxEta envsCharge
     
   concurrentlyMany_ $ [
     Draw.xterm $ Draw.topologyWithEdgeLabels System.edgeNamesOpt System.topologyOpt,
     
     putStrLn ("Number of possible flow states: " ++ show (length System.flowStatesOpt)),
     
     Draw.xterm $ Draw.flowTopologies (take 20 System.flowStatesOpt),
     
     Draw.xterm $ Draw.sequFlowGraph System.seqTopoOpt, 
--     putStrLn $ show indexCharge,
     
     PlotIO.surface "Charging" DefaultTerm.cons id noLegend varRestPower varLocalPower maxETACharge,
     PlotIO.surface "Discharging" DefaultTerm.cons id noLegend varRestPower varLocalPower maxETADischarge,
     Draw.xterm $ Draw.title "Optimise Charging" $ 
     Draw.sequFlowGraphAbsWithEnv System.seqTopoOpt $ 
     EqGen.solve System.seqTopoOpt $ Optimisation.givenCharging etaWaterCharge etaCoal etaGas etaTransHL 0.4 0.1 0 0
     ]

   report [] ("RestPower", varRestPower)
   report [] ("LocalPower", varLocalPower)
   report [] ("waterPower", varWaterPower)
   report [] ("indexChargeX", Sig.map fst indexCharge')
   report [] ("gasPower", varGasPower)
   report [] ("indexChargeY", Sig.map snd indexCharge')
