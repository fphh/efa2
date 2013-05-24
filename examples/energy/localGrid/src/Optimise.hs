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
--import Data.Tuple.HT (mapPair)

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
restPower = [0,0.05 .. 0.4]

localPower :: [Double]
localPower = [0.1,0.15 .. 0.4]

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
     
-- | Sweep all Configuration Optimations
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
-- | Avoid invalid solution by assigning NaN, which hits last in maximum
calcEtaSys env =  if eCoal0 >= 0 && eCoal1 >= 0 && eTransformer0 >= 0 && eTransformer1 >= 0 then etaSys else -0.333
     where
     eGas0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Gas LocalNetwork)) env
     eCoal0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Coal Network)) env
     eRest0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Rest Network)) env
     eRestLocal0 = (ModUt.lookupAbsEnergy (XIdx.energy sec0 LocalRest LocalNetwork)) env
     eGas1 = (ModUt.lookupAbsEnergy (XIdx.energy sec1 Gas LocalNetwork)) env
     eCoal1 =  (ModUt.lookupAbsEnergy (XIdx.energy sec1 Coal Network)) env
     eRest1 =  (lookupAbsEnergy (XIdx.energy sec1 Rest Network)) env
     eRestLocal1 =  (lookupAbsEnergy (XIdx.energy sec1 LocalRest LocalNetwork)) env
     eTransformer0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Network LocalNetwork)) env
     eTransformer1 =  (ModUt.lookupAbsEnergy (XIdx.energy sec1 Network LocalNetwork)) env

     etaSys = (eRest0 + eRest1 + eRestLocal0 + eRestLocal1) / (eGas0  + eGas1 + eCoal0 + eCoal1)
                 

maxEta :: Sig.UTSignal2 [] [] 
         (EqEnv.Complete  
         Node
         (EqRec.Absolute (Result Double))
         (EqRec.Absolute (Result Double))) ->
         (Double, 
          Maybe (EqEnv.Complete          
                 Node
                 (EqRec.Absolute (Result Double))
                 (EqRec.Absolute (Result Double))))
maxEta sigEnvs = (Sig.fromScalar etaMax, env) 
  where 
    etaSys = Sig.map calcEtaSys sigEnvs
--    etaMax = Sig.map (\x -> if isNaN x then -0.333 else x) $ Sig.maximum etaSys
    etaMax = Sig.maximum etaSys
    (xIdx, yIdx) = Sig.findIndex2 (== Sig.fromScalar etaMax) etaSys
    env = case (xIdx, yIdx) of 
      (Just xIdx', Just yIdx') -> Just $ Sig.getSample2D sigEnvs (xIdx',yIdx')
      _ -> Nothing
    
main :: IO ()
main = do

   IO.hSetEncoding IO.stdout IO.utf8
    
   tabEta <- Table.read "../simulation/maps/eta.txt"
   tabPower <- Table.read "../simulation/maps/power.txt"

-- | Import Efficiency Maps
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

-- | Import Power Curves
--   let powerWind :: Sig.PSignal [] Double
       timeWind :: Sig.TSignal [] Double        
       powerSignalWind :: Sig.PSignal [] Double
       (timeWind,[powerSignalWind]) = CT.convertToSignal2D (M.lookup "wind" tabPower)
       
       timeSolar :: Sig.TSignal [] Double        
       powerSignalSolar :: Sig.PSignal [] Double
       (timeSolar,[powerSignalSolar]) = CT.convertToSignal2D (M.lookup "solar" tabPower)
       
       timeHouse :: Sig.TSignal [] Double        
       powerSignalHouse :: Sig.PSignal [] Double
       (timeHouse,[powerSignalHouse]) = CT.convertToSignal2D (M.lookup "house" tabPower)
       
       timeIndustry :: Sig.TSignal [] Double        
       powerSignalIndustry :: Sig.PSignal [] Double
       (timeIndustry,[powerSignalIndustry]) = CT.convertToSignal2D (M.lookup "industry" tabPower)
       
       powerSignalRest = Sig.scale powerSignalWind restPowerScale
       powerSignalLocal = Sig.scale  (powerSignalSolar Sig..+ Sig.makeDelta (powerSignalHouse Sig..+ (Sig.makeDelta powerSignalIndustry))) localPowerScale

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

     etaSysMax::    Sig.NTestRow2 [] [] Double
     etaSysMax = Sig.zipWith max maxETACharge maxETADischarge
     
     maxEtaSysState :: Sig.UTTestRow2 [] [] Double
     maxEtaSysState = Sig.map fromIntegral $ Sig.sigMax2 maxETACharge maxETADischarge

     
     envsChargeOpt :: Sig.UTTestRow2 [] [] (Maybe
                      (EqEnv.Complete  
                       Node
                       (EqRec.Absolute (Result Double))
                       (EqRec.Absolute (Result Double))))
     envsChargeOpt = Sig.map snd $ Sig.map maxEta envsCharge
     
     envsDischargeOpt :: Sig.UTTestRow2 [] [] (Maybe
                      (EqEnv.Complete  
                       Node
                       (EqRec.Absolute (Result Double))
                       (EqRec.Absolute (Result Double))))
     envsDischargeOpt = Sig.map snd $ Sig.map maxEta envsDischarge

     powerGasChargeOpt :: Sig.PTestRow2 [] [] Double
     powerGasChargeOpt = Sig.setType $ Sig.map  (ModUt.lookupAbsPower (XIdx.power sec0 Gas LocalNetwork)) envsChargeOpt
       
     powerWaterChargeOpt :: Sig.PTestRow2 [] [] Double
     powerWaterChargeOpt = Sig.setType $ Sig.map  (ModUt.lookupAbsPower (XIdx.power sec0 Water Network)) envsChargeOpt
     
     powerGasDischargeOpt :: Sig.PTestRow2 [] [] Double
     powerGasDischargeOpt = Sig.setType $ Sig.map  (ModUt.lookupAbsPower (XIdx.power sec1 LocalNetwork Gas)) envsDischargeOpt
       
     powerWaterDischargeOpt :: Sig.PTestRow2 [] [] Double
     powerWaterDischargeOpt = Sig.setType $ Sig.map  (ModUt.lookupAbsPower (XIdx.power sec1 Water Network)) envsDischargeOpt
    
     powerTransformerChargeOpt :: Sig.PTestRow2 [] [] Double
     powerTransformerChargeOpt = Sig.setType $ Sig.map  (ModUt.lookupAbsPower (XIdx.power sec0 Network LocalNetwork)) envsChargeOpt
     
     powerTransformerDischargeOpt :: Sig.PTestRow2 [] [] Double
     powerTransformerDischargeOpt = Sig.setType $ Sig.map  (ModUt.lookupAbsPower (XIdx.power sec1 Network LocalNetwork)) envsDischargeOpt   
     
{-     
     powerSignalWaterOptCharge = Sig.interp2WingProfile "powerSignalWaterOptCharge" 
                                 varRestPower varLocalPower powerWaterChargeOpt powerSignalRest powerSignalLocal 
     
     powerSignalGasOptCharge = Sig.interp2WingProfile "powerSignalGasOptCharge" 
                                 varRestPower varLocalPower powerGasChargeOpt powerSignalRest powerSignalLocal 
                                 
     powerSignalWaterOptDischarge = Sig.interp2WingProfile "powerSignalWaterOptDischarge" 
                                 varRestPower varLocalPower powerWaterDischargeOpt powerSignalRest powerSignalLocal 
     
     powerSignalGasOptDischarge = Sig.interp2WingProfile "powerSignalGasOptDischarge" 
                                 varRestPower varLocalPower powerGasDischargeOpt powerSignalRest powerSignalLocal 
-}                                 
                                 
  
   concurrentlyMany_ $ [
     Draw.xterm $ Draw.topologyWithEdgeLabels System.edgeNamesOpt System.topologyOpt,
     putStrLn ("Number of possible flow states: " ++ show (length System.flowStatesOpt)),
     Draw.xterm $ Draw.flowTopologies (take 20 System.flowStatesOpt),
     Draw.xterm $ Draw.sequFlowGraph System.seqTopoOpt, 
     
     PlotIO.surface "Optimal System Efficiency " DefaultTerm.cons id noLegend varRestPower varLocalPower etaSysMax,
     PlotIO.surface "Optimal State " DefaultTerm.cons id noLegend varRestPower varLocalPower maxEtaSysState,
     
     PlotIO.surface "Charging Optimal System Efficiency " DefaultTerm.cons id noLegend varRestPower varLocalPower maxETACharge,
     PlotIO.surface "Charging Optimal Gas Power " DefaultTerm.cons id noLegend varRestPower varLocalPower powerGasChargeOpt,
     PlotIO.surface "Charging Optimal Water Power " DefaultTerm.cons id noLegend varRestPower varLocalPower powerWaterChargeOpt,
     
     PlotIO.surface "Discharging" DefaultTerm.cons id noLegend varRestPower varLocalPower maxETADischarge,
     PlotIO.surface "Discharging Optimal Gas Power " DefaultTerm.cons id noLegend varRestPower varLocalPower powerGasDischargeOpt,
     PlotIO.surface "Discharging Optimal Water Power " DefaultTerm.cons id noLegend varRestPower varLocalPower powerWaterDischargeOpt,
     
     PlotIO.surface "Transformer Power Charge HV " DefaultTerm.cons id noLegend varRestPower varLocalPower powerTransformerChargeOpt,
     PlotIO.surface "Transformer Power DisCharge HV" DefaultTerm.cons id noLegend varRestPower varLocalPower powerTransformerDischargeOpt,
     
     PlotIO.xy "Operation" DefaultTerm.cons id show powerSignalRest powerSignalLocal
     
     ]
     
     

   report [] ("RestPower", varRestPower)
   report [] ("LocalPower", varLocalPower)
   report [] ("waterPower", varWaterPower)
   report [] ("gasPower", varGasPower)

