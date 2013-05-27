{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where


import EFA.Signal.Sequence (-- genSequenceSignal,
                            addZeroCrossings,
                            genSequ,
                           -- sectionRecordsFromSequence
                           )

--import qualified Modules.Signals as Signals
import qualified Modules.System as System
import Modules.System (Node(..))
--import qualified Modules.Analysis as Analysis
import qualified Modules.Optimisation as Optimisation
import Modules.Optimisation (sec0,sec1)
import Modules.Utility as ModUt
import qualified Modules.Analysis as Analysis

import EFA.Report.Report(report,ROpt(..))

import qualified EFA.Signal.Record as Record
import EFA.Utility.Async (concurrentlyMany_)
import qualified EFA.Graph.Draw as Draw
--import EFA.IO.PLTImport (modelicaPLTImport)
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Signal (TC,Scalar)



-- import qualified EFA.Signal.Vector as V
import qualified Data.Vector as V
import EFA.Signal.Data (Data(..), Nil, (:>))
import EFA.Signal.Typ (Typ, F, T, A, Tt)
import qualified EFA.Signal.SequenceData as SD
--import qualified EFA.Utility as Utility
import EFA.Signal.Sequence (makeSeqFlowTopology)
import qualified EFA.Graph.Flow as Flow
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
import qualified EFA.Graph.Topology.Index as TIdx
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
          
-- ################### Vary Power Demand

restPower :: [Double]
restPower = [0.2,0.4 .. 1.0]

localPower :: [Double]
localPower = [0.2,0.4 .. 2.2]

varRestPower', varLocalPower' :: [[Double]]
(varLocalPower', varRestPower') = CT.varMat localPower restPower 

restPowerScale :: Double
restPowerScale = 1

varRestPower1D :: Sig.PSignal V.Vector Double
varRestPower1D = Sig.fromList restPower

localPowerScale :: Double
localPowerScale = 1

varRestPower :: Sig.PSignal2 V.Vector V.Vector Double
varRestPower = Sig.fromList2 varRestPower'

varLocalPower :: Sig.PSignal2 V.Vector V.Vector Double
varLocalPower = Sig.fromList2 varLocalPower'


scaleTableEta :: M.Map String (Double, Double)
scaleTableEta = M.fromList $
  ("storage",     (1, 1)) :
  ("gas",         (powerScaleGas, 1)) :
  ("transformer", (powerScaleTransformer, 1)) :
  ("coal",        (powerScaleCoal, 1)) :
  ("local",       (1, 1)) :
  ("rest",        (1, 1)) :
  []


{-
m ::
  (TIdx.Flip (TIdx.InSection TIdx.Eta)) =>
  TIdx.Section ->
  M.Map (TIdx.InSection TIdx.Eta Node) (String, TIdx.InSection TIdx.Eta Node -> TIdx.InSection TIdx.Eta Node)
-}
m :: TIdx.Section -> M.Map (XIdx.Eta Node) (String, XIdx.Eta Node -> XIdx.Power Node)
m sec = M.fromList $
  (XIdx.eta sec Water Network, ( "storage",myflip)) :
  (XIdx.eta sec Network Water, ( "storage", noflip)) :
  (XIdx.eta sec Coal Network, ( "coal", myflip)) :
  (XIdx.eta sec Gas LocalNetwork, ( "gas", myflip)) :
  (XIdx.eta sec Network LocalNetwork, ( "transformer", myflip)) :
  (XIdx.eta sec LocalNetwork Network, ( "transformer", noflip)) :
  (XIdx.eta sec LocalNetwork LocalRest, ( "local", myflip)) :
  (XIdx.eta sec Network Rest, ( "rest", myflip)) :
  []

noflip :: TIdx.InSection TIdx.Eta node -> TIdx.InSection TIdx.Power node
noflip (TIdx.InSection sec ( TIdx.Eta (TIdx.StructureEdge n1 n2)))  = (TIdx.InSection sec (TIdx.Power (TIdx.StructureEdge n1 n2))) 


myflip :: TIdx.InSection TIdx.Eta node -> TIdx.InSection TIdx.Power node
myflip (TIdx.InSection sec ( TIdx.Eta (TIdx.StructureEdge n1 n2)))  = (TIdx.InSection sec (TIdx.Power (TIdx.StructureEdge n2 n1))) 


-- ################### Vary Degrees of Freedom for Optimisation

waterPower :: [Double]
waterPower = [0.2,0.4 .. 0.8]

gasPower :: [Double]
gasPower = [0.2,0.4 .. 1.0]

powerScaleWater ::  Double
powerScaleWater = 1.5
  
powerScaleGas ::  Double
powerScaleGas = 1

powerScaleTransformer ::  Double
powerScaleTransformer = 2

powerScaleCoal ::  Double
powerScaleCoal = 5  
  
varWaterPowerSig :: Sig.PTestRow [] Double
varWaterPowerSig = Sig.fromList waterPower

varGasPowerSig :: Sig.PTestRow [] Double
varGasPowerSig = Sig.fromList gasPower

varWaterPower', varGasPower' :: [[Double]]
(varWaterPower', varGasPower') = CT.varMat waterPower gasPower

varWaterPower :: Sig.PSignal2 V.Vector V.Vector Double
varWaterPower = Sig.fromList2 varWaterPower'

varGasPower :: Sig.PSignal2 V.Vector V.Vector Double
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
                Sig.UTSignal2 V.Vector V.Vector 
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
                  Sig.UTSignal2 V.Vector V.Vector
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
                 

maxEta :: Sig.UTSignal2 V.Vector V.Vector 
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
   let 
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
     envsCharge :: Sig.UTSignal2 V.Vector V.Vector (Sig.UTSignal2 V.Vector V.Vector
                         (EqEnv.Complete  
                           Node
                           (EqRec.Absolute (Result  Double))
                           (EqRec.Absolute (Result  Double))))
     envsCharge = Sig.fromList2 $ zipWith (zipWith (\ x y -> sweepCharge etaWaterCharge etaCoal etaGas etaTransHL x y varWaterPower' varGasPower')) varRestPower' varLocalPower'


     envsDischarge :: Sig.UTSignal2 V.Vector V.Vector (Sig.UTSignal2 V.Vector V.Vector
                           (EqEnv.Complete  
                             Node
                             (EqRec.Absolute (Result Double))
                             (EqRec.Absolute (Result Double))))
     envsDischarge = Sig.fromList2 $ zipWith (zipWith (\ x y -> sweepDischarge etaWaterCharge etaCoal etaGas etaTransHL x y varWaterPower' varGasPower')) varRestPower' varLocalPower'

     maxETACharge :: Sig.NSignal2 V.Vector V.Vector Double
     maxETACharge = Sig.setType $ Sig.map fst $ Sig.map maxEta envsCharge
     maxETADischarge :: Sig.NSignal2 V.Vector V.Vector Double
     maxETADischarge = Sig.setType $ Sig.map fst $ Sig.map maxEta envsDischarge

     etaSysMax::    Sig.NSignal2 V.Vector V.Vector Double
     etaSysMax = Sig.zipWith max maxETACharge maxETADischarge
     
     maxEtaSysState :: Sig.UTSignal2 V.Vector V.Vector Double
     maxEtaSysState = Sig.map fromIntegral $ Sig.sigMax2 maxETACharge maxETADischarge

     
     envsChargeOpt :: Sig.UTSignal2 V.Vector V.Vector (Maybe
                      (EqEnv.Complete  
                       Node
                       (EqRec.Absolute (Result Double))
                       (EqRec.Absolute (Result Double))))
     envsChargeOpt = Sig.map snd $ Sig.map maxEta envsCharge
     
     envsDischargeOpt :: Sig.UTSignal2 V.Vector V.Vector (Maybe
                      (EqEnv.Complete  
                       Node
                       (EqRec.Absolute (Result Double))
                       (EqRec.Absolute (Result Double))))
     envsDischargeOpt = Sig.map snd $ Sig.map maxEta envsDischarge

     powerGasChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerGasChargeOpt = Sig.setType $ Sig.map  (ModUt.lookupAbsPower (XIdx.power sec0 Gas LocalNetwork)) envsChargeOpt
       
     powerWaterChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerWaterChargeOpt = Sig.setType $ Sig.map  (ModUt.lookupAbsPower (XIdx.power sec0 Water Network)) envsChargeOpt
     
     powerGasDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerGasDischargeOpt = Sig.setType $ Sig.map  (ModUt.lookupAbsPower (XIdx.power sec1 LocalNetwork Gas)) envsDischargeOpt
       
     powerWaterDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerWaterDischargeOpt = Sig.setType $ Sig.map  (ModUt.lookupAbsPower (XIdx.power sec1 Water Network)) envsDischargeOpt
    
     powerTransformerChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerTransformerChargeOpt = Sig.setType $ Sig.map  (ModUt.lookupAbsPower (XIdx.power sec0 Network LocalNetwork)) envsChargeOpt
     
     powerTransformerDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerTransformerDischargeOpt = Sig.setType $ Sig.map  (ModUt.lookupAbsPower (XIdx.power sec1 Network LocalNetwork)) envsDischargeOpt   
      

     powerSignalWaterOptCharge =
       Sig.tzipWith
         (\x y -> Sig.interp2WingProfile
           "powerSignalWaterOptCharge" 
           varRestPower1D varLocalPower powerWaterChargeOpt x y)
         powerSignalRest powerSignalLocal

     powerSignalGasOptCharge =
       Sig.tzipWith
         (\x y -> Sig.interp2WingProfile
           "powerSignalGasOptCharge" 
           varRestPower1D varLocalPower powerGasChargeOpt x y)
         powerSignalRest powerSignalLocal

     powerSignalWaterOptDischarge =
       Sig.tzipWith
         (\x y -> Sig.interp2WingProfile
           "powerSignalWaterOptDischarge" 
           varRestPower1D varLocalPower powerWaterDischargeOpt x y)
         powerSignalRest powerSignalLocal

     powerSignalGasOptDischarge =
       Sig.tzipWith
         (\x y -> Sig.interp2WingProfile
           "powerSignalGasOptDischarge" 
           varRestPower1D varLocalPower powerGasDischargeOpt x y)
         powerSignalRest powerSignalLocal

     stateSignal =
       Sig.tzipWith
         (\x y -> Sig.interp2WingProfile
           "stateSignal" 
           varRestPower1D varLocalPower maxEtaSysState x y)
         powerSignalRest powerSignalLocal
         
     powerSignalWater = Sig.zipWith (\state (x,y) -> if state==0 then -x else y) 
                        stateSignal (Sig.zip powerSignalWaterOptCharge powerSignalWaterOptDischarge)   
     
     powerSignalGas = Sig.zipWith (\state (x,y) -> if state==0 then x else y) 
                        stateSignal (Sig.zip powerSignalGasOptCharge powerSignalGasOptDischarge)   
                                 
     time :: Sig.TSignal [] Double
     time = Sig.fromList [0..23]
     rec = Record.Record time $ 
           M.fromList [(TIdx.PPos (TIdx.StructureEdge Rest Network), powerSignalRest), 
                       (TIdx.PPos (TIdx.StructureEdge LocalRest LocalNetwork), powerSignalLocal),
                       (TIdx.PPos (TIdx.StructureEdge Network Water), powerSignalWater), 
                       (TIdx.PPos (TIdx.StructureEdge LocalNetwork Gas), powerSignalGas)
                      ]
     
     -- | Sequenceflow from Selected Section
     seqTopoSim = Flow.mkSequenceTopology (ModUt.select System.flowStatesOpt [4])
     -- | Table with efficiency maps 
     mstr = CT.makeEtaFunctions2D scaleTableEta tabEta
     
     -- | Generated Equation System
     eqs = Optimisation.givenSimulate m mstr $ 
           SD.SequData [(SD.Section (TIdx.Section 0) (Sig.SignalIdx 0, Sig.SignalIdx 23) rec)] --sequencePowers
           
     -- | Simulation Result as Env      
     envSim = EqGen.solve seqTopoSim eqs
 
     powerRecSim = ModUt.envToPowerRecord envSim time 0
     
     flipwater (TIdx.PPos (TIdx.StructureEdge Network Water)) x = Sig.neg x  --Sig.neg x 
     flipwater (TIdx.PPos (TIdx.StructureEdge Water Network)) x = Sig.neg x
     flipwater _ x = x
     powerRecSimCorr = Record.rmapWithKey flipwater powerRecSim
     
     

     rec0 = addZeroCrossings powerRecSimCorr
       
     sectionFilterTime ::  TC Scalar (Typ A T Tt) (Data Nil Double)
     sectionFilterTime = Sig.toScalar 0

     sectionFilterEnergy ::  TC Scalar (Typ A F Tt) (Data Nil Double)
     sectionFilterEnergy = Sig.toScalar 0

     sequencePowers :: SD.SequData (Record.PowerRecord System.Node [] Double)
     sequencePowers = genSequ rec0
     (sequencePowersFilt, sequenceFlowsFilt) =
         SD.unzip $
         SD.filter (Record.major sectionFilterEnergy sectionFilterTime . snd) $
         fmap (\x -> (x, Record.partIntegrate x)) sequencePowers
   
     (flowStates, adjustedFlows) =
         SD.unzip $
         fmap
         (\state ->
           let flowState = Flow.genFlowState state
           in  (flowState, Flow.adjustSigns System.topologyOpt flowState state))
         sequenceFlowsFilt

     flowTopos = Flow.genSequFlowTops System.topologyOpt flowStates
     sequenceFlowTopologySim = makeSeqFlowTopology flowTopos
     envSimAnalysis = Analysis.external2 sequenceFlowTopologySim sequenceFlowsFilt
     envSimAnalysisCumulated = Analysis.external2 sequenceFlowTopologySim (fmap Record.sumFlowRecord sequenceFlowsFilt)

    
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
     
--     PlotIO.surface "Discharging" DefaultTerm.cons id noLegend varRestPower varLocalPower maxETADischarge,
     PlotIO.surface "Discharging Optimal Gas Power " DefaultTerm.cons id noLegend varRestPower varLocalPower powerGasDischargeOpt,
     PlotIO.surface "Discharging Optimal Water Power " DefaultTerm.cons id noLegend varRestPower varLocalPower powerWaterDischargeOpt,
     
--     PlotIO.surface "Transformer Power Charge HV " DefaultTerm.cons id noLegend varRestPower varLocalPower powerTransformerChargeOpt,
--     PlotIO.surface "Transformer Power DisCharge HV" DefaultTerm.cons id noLegend varRestPower varLocalPower powerTransformerDischargeOpt,
     
     PlotIO.xy "Operation" DefaultTerm.cons id show powerSignalRest powerSignalLocal,
     
     report [] ("RestPower", varRestPower),
     report [] ("LocalPower", varLocalPower),
     report [] ("waterPower", varWaterPower),
     report [] ("gasPower", varGasPower),
     Draw.xterm $ Draw.sequFlowGraphAbsWithEnv  seqTopoSim envSim,
     report [] ("powerRecordSim",powerRecSim),
     PlotIO.record "Calculated Signals" DefaultTerm.cons show id rec,
     PlotIO.record "Simulation Result" DefaultTerm.cons show id powerRecSim,
     
     PlotIO.signal "State"  DefaultTerm.cons id stateSignal ,
     PlotIO.signal "Interpolated Signals"  DefaultTerm.cons id [powerSignalWaterOptCharge, 
                                               powerSignalWaterOptDischarge,
                                               powerSignalGasOptCharge, 
                                               powerSignalGasOptDischarge],
     Draw.xterm $ Draw.sequFlowGraphAbsWithEnv  sequenceFlowTopologySim envSimAnalysis,
     Draw.xterm $ Draw.sequFlowGraphAbsWithEnv  sequenceFlowTopologySim envSimAnalysisCumulated
     
     ]
     
     
