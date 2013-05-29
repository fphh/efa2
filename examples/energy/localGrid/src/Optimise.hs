{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where



import qualified Modules.System as System
import Modules.System (Node(..))
import qualified Modules.Optimisation as Optimisation
import Modules.Optimisation (sec0,sec1, maxEta, calcEtaSys)
import Modules.Utility as ModUt
import qualified Modules.Analysis as Analysis
import Modules.Utility(getEtas, getTimes,select)


import EFA.Signal.Sequence (-- genSequenceSignal,
                            addZeroCrossings,
                            genSequ,
                           -- sectionRecordsFromSequence
                           )
import EFA.Report.Report(report,ROpt(..))
import qualified EFA.Signal.Record as Record
import EFA.Utility.Async (concurrentlyMany_)
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Signal (TC,Scalar)
import EFA.Signal.Data (Data(..), Nil, (:>), getData)
import EFA.Signal.Typ (Typ, F, T, A, Tt)
import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.Sequence (makeSeqFlowTopology)
import qualified EFA.Graph.Flow as Flow
--import qualified EFA.Graph.Draw as Draw
--import qualified EFA.Signal.Plot as Plot
import qualified EFA.Example.Index as XIdx
--import qualified EFA.Equation.Arithmetic as Arith
--import qualified EFA.Equation.Environment as Env
import qualified EFA.Example.Absolute as EqGen
--import qualified EFA.Example.Index as XIdx
import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.Record as EqRec
import EFA.Equation.Result (Result(..))
--import EFA.Utility.Map (checkedLookup)
import qualified EFA.Signal.PlotIO as PlotIO
import qualified EFA.Signal.ConvertTable as CT
import qualified EFA.IO.TableParser as Table
--import qualified EFA.IO.TableParserTypes as TPT
import qualified EFA.Signal.Vector as SV

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified System.IO as IO
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.GraphViz.Attributes.Colors.X11 as Colors


-- ################### Plot Stuff

plotTerm :: DefaultTerm.T
plotTerm = DefaultTerm.cons

colours :: [Colors.X11Color]
colours = [ Colors.White,	
            Colors.Gray90,	
            Colors.Gray80,	
            Colors.Gray70 ]
          
noLegend :: Int -> String
noLegend =  (const "")

legend :: Int -> String
legend 0 = "Laden"
legend 1 = "Entladen"
legend _ = "Undefined"


scaleTableEta :: M.Map String (Double, Double)
scaleTableEta = M.fromList $
  ("storage",     (1, 1)) :
  ("gas",         (powerScaleGas, 1)) :
  ("transformer", (powerScaleTransformer, 1)) :
  ("coal",        (powerScaleCoal, 1)) :
  ("local",       (1, 1)) :
  ("rest",        (1, 1)) :
  []

-- ################### Efficiency Curves

etaAssign :: TIdx.Section -> M.Map (XIdx.Eta Node) (String, XIdx.Eta Node -> XIdx.Power Node)
etaAssign sec = M.fromList $
  (XIdx.eta sec Water Network, ( "storage",myflip)) :
  (XIdx.eta sec Network Water, ( "storage", noflip)) :

  (XIdx.eta sec Coal Network, ( "coal", myflip)) :
  (XIdx.eta sec Network Coal, ( "coal", noflip)) :

  (XIdx.eta sec Gas LocalNetwork, ( "gas", myflip)) :
  (XIdx.eta sec LocalNetwork Gas, ( "gas", noflip)) :


  (XIdx.eta sec Network LocalNetwork, ( "transformer", myflip)) :
  (XIdx.eta sec LocalNetwork Network, ( "transformer", noflip)) :

  (XIdx.eta sec LocalNetwork LocalRest, ( "local", myflip)) :
  (XIdx.eta sec LocalRest LocalNetwork, ( "local", noflip)) :

  (XIdx.eta sec Network Rest, ( "rest", myflip)) :
  (XIdx.eta sec Rest Network, ( "rest", noflip)) :

  []

noflip :: TIdx.InSection TIdx.Eta node -> TIdx.InSection TIdx.Power node
noflip (TIdx.InSection sec ( TIdx.Eta (TIdx.StructureEdge n1 n2)))  = (TIdx.InSection sec (TIdx.Power (TIdx.StructureEdge n1 n2))) 


myflip :: TIdx.InSection TIdx.Eta node -> TIdx.InSection TIdx.Power node
myflip (TIdx.InSection sec ( TIdx.Eta (TIdx.StructureEdge n1 n2)))  = (TIdx.InSection sec (TIdx.Power (TIdx.StructureEdge n2 n1))) 

-- ################### Vary Power Demand

restPower :: [Double]
restPower = [0.2,0.4 .. 2]

localPower :: [Double]
localPower = [0.2,0.4 .. 2.8]

varRestPower', varLocalPower' :: [[Double]]
(varLocalPower', varRestPower') = CT.varMat localPower restPower 

restPowerScale :: Double
restPowerScale = 1

varRestPower1D :: Sig.PSignal V.Vector Double
varRestPower1D = Sig.fromList restPower

localPowerScale :: Double
localPowerScale = 1.2

varRestPower :: Sig.PSignal2 V.Vector V.Vector Double
varRestPower = Sig.fromList2 varRestPower'

varLocalPower :: Sig.PSignal2 V.Vector V.Vector Double
varLocalPower = Sig.fromList2 varLocalPower'


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
powerScaleTransformer = 2.5

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

sweep ::(SV.Storage c2 d,
          SV.Storage c1 (c2 d), SV.FromList c2,
          SV.FromList c1) =>
         (a -> b -> d)
         -> [[a]] -> [[b]] -> TC s t (Data (c1 :> (c2 :> Nil)) d)
sweep func xs ys = Sig.fromList2 $ zipWith (zipWith func) xs ys

doubleSweep ::
  Optimisation.SolveFunc Double ->
  M.Map String (Double -> Double) ->
  [[Double]] ->
  [[Double]] ->
  [[Double]] -> 
  [[Double]] ->
  Sig.UTSignal2 V.Vector V.Vector
    (Sig.UTSignal2 V.Vector V.Vector
      (EqEnv.Complete 
        Node
        (EqRec.Absolute (Result (Data Nil Double)))
        (EqRec.Absolute (Result (Data Nil Double)))))
doubleSweep func etaFunc varOptX varOptY varX varY= 
  sweep f (mm varX) (mm varY)
  where f x y =
          sweep (func etaAssign etaFunc x y) xo yo
        xo = mm varOptX
        yo = mm varOptY
        mm = map (map Data)


main :: IO ()
main = do

   IO.hSetEncoding IO.stdout IO.utf8
    
   tabEta <- Table.read "../simulation/maps/eta.txt"
   tabPower <- Table.read "../simulation/maps/power.txt"
   
   
   -- |Import Efficiency Curves
   let etaFunc = CT.makeEtaFunctions2D scaleTableEta tabEta
       etaWaterCharge:etaWaterDischarge:etaGas:etaCoal:etaTransHL:_ =
         getEtas etaFunc ["storage", "storage", "gas", "coal", "transformer"]

   -- | Import Power Curves
   let (timeWind, powerSignalWind) :
         (timeSolar, powerSignalSolar) :
         (timeHouse, powerSignalHouse) :
         (timeIndustry, powerSignalIndustry) : _
           = getTimes tabPower ["wind", "solar", "house", "industry"]

  
       powerSignalRest = Sig.scale powerSignalWind restPowerScale
       powerSignalLocal = Sig.scale  (powerSignalSolar Sig..+ 
                                      Sig.makeDelta (powerSignalHouse Sig..+ 
                                                     (Sig.makeDelta powerSignalIndustry))) 
                          localPowerScale

   let  
     envFmap (EqEnv.Complete scal sig) =
       EqEnv.Complete (fmap gFmap scal) (fmap gFmap sig)
     gFmap = fmap $ fmap getData

     
    -- | Speep optimisation and operation space for charge and discharge case 
     envsCharge ::
       Sig.UTSignal2 V.Vector V.Vector
         (Sig.UTSignal2 V.Vector V.Vector
           (EqEnv.Complete  
              Node
              (EqRec.Absolute (Result Double))
              (EqRec.Absolute (Result Double))))
     envsCharge =  Sig.map (Sig.map envFmap) $ 
       doubleSweep Optimisation.solveCharge etaFunc varWaterPower' varGasPower' varRestPower' varLocalPower'

     envsDischarge ::
       Sig.UTSignal2 V.Vector V.Vector
         (Sig.UTSignal2 V.Vector V.Vector
           (EqEnv.Complete 
             Node
             (EqRec.Absolute (Result Double))
             (EqRec.Absolute (Result Double))))
     envsDischarge = Sig.map (Sig.map envFmap) $ 
       doubleSweep Optimisation.solveDischarge etaFunc varWaterPower' varGasPower' varRestPower' varLocalPower'

     -- | Get maximum Efficiency Envelope for charge and discharge
     maxETACharge :: Sig.NSignal2 V.Vector V.Vector Double
     maxETACharge = Sig.setType $ Sig.map fst $ Sig.map maxEta envsCharge
     
     maxETADischarge :: Sig.NSignal2 V.Vector V.Vector Double
     maxETADischarge = Sig.setType $ Sig.map fst $ Sig.map maxEta envsDischarge

     -- | Get maximum efficiency for both cases 
     etaSysMax::    Sig.NSignal2 V.Vector V.Vector Double
     etaSysMax = Sig.zipWith max maxETACharge maxETADischarge
     
     maxEtaSysState :: Sig.UTSignal2 V.Vector V.Vector Double
     maxEtaSysState = Sig.map fromIntegral $ Sig.sigMax2 maxETACharge maxETADischarge

     
     -- | Get the correspondig optimal envs for both states
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

     -- | Extract interesting Variables
     powerGasChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerGasChargeOpt = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec0 Gas LocalNetwork)) envsChargeOpt
       
     powerWaterChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerWaterChargeOpt = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec0 Water Network)) envsChargeOpt
     
     powerGasDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerGasDischargeOpt = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec1 LocalNetwork Gas)) envsDischargeOpt
       
     powerWaterDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerWaterDischargeOpt = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec1 Water Network)) envsDischargeOpt
    
     powerTransformerChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerTransformerChargeOpt = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec0 Network LocalNetwork)) envsChargeOpt
     
     powerTransformerDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerTransformerDischargeOpt = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec1 Network LocalNetwork)) envsDischargeOpt   
      
     powerTransformerChargeOptLV :: Sig.PSignal2 V.Vector V.Vector Double
     powerTransformerChargeOptLV = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec0 LocalNetwork Network)) envsChargeOpt
     
     powerTransformerDischargeOptLV :: Sig.PSignal2 V.Vector V.Vector Double
     powerTransformerDischargeOptLV = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec1 LocalNetwork Network)) envsDischargeOpt   
       
     ------------------------------------------------------------------------------------------
     -- | Start Simulation Here  
     
     -- | Generate optimal control signals and build power record

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
     
     -- | Build Sequenceflow graph for simulation 
     seqTopoSim = Flow.mkSequenceTopology (ModUt.select System.flowStatesOpt [4])
     
     -- | Generate and solve Equation System
     eqs = Optimisation.givenSimulate etaAssign etaFunc $ 
           SD.SequData [(SD.Section (TIdx.Section 0) (Sig.SignalIdx 0, Sig.SignalIdx 23) rec)]
     envSim = EqGen.solve seqTopoSim eqs
     
     -- | extract power record from simulation env to allow subsequent EFA
     powerRecSim = ModUt.envToPowerRecord envSim time 0
     
     -- | flip signs of power signals at water edge, as edge flips direction between state 0 and 4
     flipwater (TIdx.PPos (TIdx.StructureEdge Network Water)) x = Sig.neg x  --Sig.neg x 
     flipwater (TIdx.PPos (TIdx.StructureEdge Water Network)) x = Sig.neg x
     flipwater _ x = x
     powerRecSimCorr = Record.rmapWithKey flipwater powerRecSim
     
     -- | make efa on simulation results  
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
     envSimAnalysisCumulated = Analysis.external2 sequenceFlowTopologySim
                                 (fmap Record.sumFlowRecord sequenceFlowsFilt)

{-
   concurrentlyMany_ [
     Draw.xterm $ Draw.sequFlowGraphAbsWithEnv System.seqTopoOpt
                  (Sig.getSample2D (Sig.getSample2D envsCharge (Sig.SignalIdx 1, Sig.SignalIdx 1)) (Sig.SignalIdx 1, Sig.SignalIdx 1)),
     Draw.xterm $ Draw.sequFlowGraphAbsWithEnv System.seqTopoOpt
                  (Sig.getSample2D (Sig.getSample2D envsDischarge (Sig.SignalIdx 1, Sig.SignalIdx 1)) (Sig.SignalIdx 1, Sig.SignalIdx 1)) ]
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
     
--     PlotIO.surface "Discharging" DefaultTerm.cons id noLegend varRestPower varLocalPower maxETADischarge,
     PlotIO.surface "Discharging Optimal Gas Power " DefaultTerm.cons id noLegend varRestPower varLocalPower powerGasDischargeOpt,
     PlotIO.surface "Discharging Optimal Water Power " DefaultTerm.cons id noLegend varRestPower varLocalPower powerWaterDischargeOpt,
     
     PlotIO.surface "Transformer Power Charge HV " DefaultTerm.cons id noLegend varRestPower varLocalPower powerTransformerChargeOpt,
     PlotIO.surface "Transformer Power DisCharge HV" DefaultTerm.cons id noLegend varRestPower varLocalPower powerTransformerDischargeOpt,
     
     PlotIO.surface "Transformer Power Charge LV " DefaultTerm.cons id noLegend varRestPower varLocalPower powerTransformerChargeOptLV,
     PlotIO.surface "Transformer Power DisCharge LV" DefaultTerm.cons id noLegend varRestPower varLocalPower powerTransformerDischargeOptLV,
     
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

