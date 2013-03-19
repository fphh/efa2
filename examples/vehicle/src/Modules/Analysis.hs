{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}


module Modules.Analysis where

import EFA.Example.Utility (edgeVar, checkDetermined,
                            (.=),
                            (%=)
                           )
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Variable as Term
import qualified EFA.Equation.Result as R

import qualified EFA.Signal.SequenceData as SD

import EFA.Signal.Record (PPosIdx(PPosIdx), SignalRecord, FlowRecord,
                          Record(Record), PowerRecord,
                          SignalRecord,getTime, newTimeBase, removeZeroNoise)

import EFA.Signal.Sequence (genSequenceSignal,
                            removeLowEnergySections, genSequFlow, addZeroCrossings, removeLowTimeSections, 
                            genSequ,sectionRecordsFromSequence)

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Signal.Vector as Vec

import qualified EFA.Signal.Signal as Sig 
import qualified EFA.Equation.Stack as Stack
import EFA.Equation.Stack (Stack)

--import qualified EFA.Report.Report as Rep 

--import EFA.Signal.Typ
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Flow as Flow
import qualified Data.Map as M
import Data.Monoid ((<>),mempty)
import Data.Foldable (fold,
                      foldMap)

import qualified EFA.Equation.Environment as Env
import EFA.Equation.Result (Result(..))
import qualified EFA.Signal.Record as Record 
----------------------------------
-- * Example Specific Imports
import qualified Modules.System as System
import Modules.Signals as Signals
--import Modules.Plots as Plots
import qualified EFA.Graph.Topology as TD

import qualified EFA.Equation.Record as EqRecord



-------------------------------------------------------------------------------------------------

sec2 :: Idx.Section
sec2 = Idx.Section 2

-------------------------------------------------------------------------------------------------  
-- ## Preprocessing of Signals

pre :: Monad m =>
     TD.Topology System.Node
     -> SignalRecord [] Double
     -> m (SD.Sequ,
           SD.SequData (PowerRecord System.Node [] Double),
           SD.SequData (FlowRecord System.Node [] Double),
           SD.SequData (Record.FlowState System.Node), 
           PowerRecord System.Node [] Double,
           SignalRecord 
           [] Double)
pre topology rawSignals =  do

---------------------------------------------------------------------------------------
-- * Condition Signals, Calculate Powers, Remove ZeroNoise
  
  let signals = Signals.condition rawSignals
  let powerSignals = removeZeroNoise (Signals.calculatePower signals) (10^^(-2::Int))

---------------------------------------------------------------------------------------
-- * Add zerocrossings in Powersignals and Signals
      
  let powerSignals0 = addZeroCrossings powerSignals
  let signals0 = newTimeBase signals (getTime powerSignals0)

  -- Rep.report [] ("Time",(getTime powerSignals0))
  -- Rep.report [] ("Signals0",signals0)

---------------------------------------------------------------------------------------
-- * Plot Signals
{-
  Plots.vehicle signals0
  Plots.motor signals0
  Plots.generator signals0
  Plots.driveline signals0
  Plots.electric signals0
  Plots.battery signals0

  Rep.report [] ("Signals0",signals0)

---------------------------------------------------------------------------------------
-- * Plot Power Signals

  Plots.genPowers powerSignals0
  Plots.propPowers powerSignals0
  Plots.vehPowers powerSignals0
-}
---------------------------------------------------------------------------------------
-- * Cut Signals and filter on low time sektions

  let sequencePowersRaw :: SD.SequData (PowerRecord System.Node [] Double)
      (sequenceRaw,sequencePowersRaw) = genSequ powerSignals0

-- Rep.report [] ("Sequence", sequ)

  let (sequ,sequencePowers) = removeLowTimeSections(sequenceRaw,sequencePowersRaw) 0
  --  let (sequ,sequencePowers) = removeZeroTimeSections(sequenceRaw,sequencePowersRaw)

  -- create sequence signal
  let sequSig = Sig.scale (genSequenceSignal sequ) 10 :: Sig.UTSigL  --  (10  ^^ (-12::Int))
  let sequenceSignals = sectionRecordsFromSequence signals0 sequ

  --Pl.recordSplitPlus 1 "Mit SektionsSignal" powerSignals0 [(PPosIdx System.Tank System.Tank, Sig.setType sequSig)]
  --Rep.report [Rep.RAll,Rep.RVertical] ("Powers0", powerSignals0)

---------------------------------------------------------------------------------------
-- * Integrate Power and Sections on maximum Energyflow

  let sequenceFlows = genSequFlow sequencePowers

  let (sequenceFilt,sequencePowersFilt,sequenceFlowsFilt) =
        removeLowEnergySections (sequ,sequencePowers,sequenceFlows) 0

  let (flowStates, adjustedFlows) =
         SD.unzip $
         fmap
            (\state ->
               let flowState = Flow.genFlowState state
               in  (flowState, Flow.adjustSigns topology flowState state))
            sequenceFlowsFilt

  return (sequenceFilt,sequencePowersFilt,adjustedFlows, flowStates, powerSignals0, signals0)

-------------------------------------------------------------------------------------------------  
-- ## Analyse External Energy Flow
  
external :: (Eq v, Num v, Arith.Product v, Arith.Integrate v, Vec.Storage t v,
             Vec.FromList t, Arith.Scalar v ~ Double) =>
            TD.SequFlowGraph System.Node
            -> SD.SequData (Record s t2 t1 (PPosIdx System.Node) t v)
            -> Env.Complete
            System.Node
            (EqRecord.Absolute (Result Double))
            (EqRecord.Absolute (Result v))
external sequenceFlowTopology sequFlowRecord =  EqGen.solveFromMeasurement sequenceFlowTopology $ makeGivenFromExternal Idx.Absolute sequFlowRecord


initStorage :: (Fractional a, Num a) => a
initStorage = 0.7*3600*1000

makeGivenFromExternal :: (Eq v, Num v, Arith.Sum v, Vec.Storage t v, Vec.FromList t,
                          EqGen.Record (EqRecord.FromIndex rec),
                          EqRecord.ToIndex (EqRecord.FromIndex rec) ~ rec) =>
                         rec
                         -> SD.SequData (Record s t2 t1 (PPosIdx System.Node) t v)
                         -> EqGen.EquationSystem
                         (EqRecord.FromIndex rec) System.Node s1 Double v
makeGivenFromExternal idx sf =
   (Idx.Record idx (Idx.DTime Idx.initSection) .= 1)
   <> (Idx.Record idx (Idx.Storage (Idx.SecNode Idx.initSection System.Battery)) .= initStorage)
   <> (Idx.Record idx (Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia)) .= 0)
   <> fold (SD.zipWithSecIdxs f sf)
   where f sec (Record t xs) =
           (Idx.Record idx (Idx.DTime sec) .= sum (Sig.toList t)) <>
           fold (M.mapWithKey g xs)
           where g (PPosIdx a b) e =
                    Idx.Record idx (edgeVar Idx.Energy sec a b) .= sum (Sig.toList e)

-------------------------------------------------------------------------------------------------  
-- ## Predict Energy Flow

-- prediction :: TD.SequFlowGraph System.Node
--                              -> Env.Complete
--                                   System.Node
--                                   (Env.Absolute (Result Double))
--                                   (Env.Absolute (Result Double))
--                              -> Env.Complete
--                                   System.Node
--                                   (Env.Absolute (Result Double))
--                                   (Env.Absolute (Result Double))


prediction sequenceFlowTopology env = EqGen.solve sequenceFlowTopology (makeGivenForPrediction Idx.Absolute env) 

-- makeGivenForPrediction ::
--    (EqGen.Record rec) =>
--    Env.RecordIndex rec ->
--    Env.Complete System.Node
--       (rec (EqGen.Result Double)) (rec (EqGen.Result Double)) ->
--    (EqGen.EquationSystem rec System.Node s Double Double)
makeGivenForPrediction idx env =
    (Idx.Record idx (Idx.DTime Idx.initSection) .= 1)
    <> (Idx.Record idx (Idx.Storage (Idx.SecNode Idx.initSection System.Battery)) .= initStorage)
    <> (Idx.Record idx (Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia)) .= 0)
    <> (foldMap f $ M.toList $ Env.etaMap $ Env.signal env)
    <> (foldMap f $ M.toList $ Env.dtimeMap $ Env.signal env)
    <> (foldMap f $ M.toList $ M.mapWithKey h $ M.filterWithKey g $
                               Env.energyMap $ Env.signal env)
    where f (i, x)  =  i %= fmap (\(EqGen.Determined y) -> y) x
          g (Idx.Energy (Idx.StructureEdge _ x y)) _  =
             case (x,y) of
                (System.Resistance, System.Chassis) -> True
                (System.VehicleInertia, System.Chassis) -> True
                (System.RearBrakes, System.Chassis) -> True
                (System.FrontBrakes, System.ConFrontBrakes) -> True
                (System.ConES, System.ElectricSystem) -> True
                (System.Battery, System.ConBattery) -> True
                _ -> False
          h (Idx.Energy (Idx.StructureEdge _ System.Resistance System.Chassis)) x =
               fmap (fmap (*1.1)) x
          h _ r = r


---------------------------------------------------------------------------------------------------
-- ## Make Delta 

delta :: (Eq v, Num v, Arith.Product v, Arith.Integrate v, Vec.Storage t3 v,
          Vec.Storage t v, Vec.FromList t3, Vec.FromList t,
          Arith.Scalar v ~ Double) =>
         TD.SequFlowGraph System.Node
         -> SD.SequData (Record s t2 t1 (PPosIdx System.Node) t v)
         -> SD.SequData (Record s1 t5 t4 (PPosIdx System.Node) t3 v)
         -> Env.Complete
         System.Node
         (EqRecord.Delta (Result Double))
         (EqRecord.Delta (Result v))
delta sequenceFlowTopology sequenceFlow sequenceFlow'= EqGen.solveFromMeasurement sequenceFlowTopology 
                                                       $ (makeGivenFromExternal Idx.Before sequenceFlow <>
                                                          makeGivenFromExternal Idx.After sequenceFlow')

---------------------------------------------------------------------------------------------------
-- ## Make Difference Analysis

{-
infixr 6 =<>

(=<>) ::
   (Ord (idx System.Node), Env.AccessMap idx,
    Var.Index idx, Var.Type idx ~ Var.Signal) =>
   (Idx.Record Idx.Delta (idx System.Node), Double) ->
   EquationSystem s -> EquationSystem s

(idx, x) =<> eqsys =
   (idx .= Term.Signal (point (HSt.Symbol (fmap Var.index idx) x))) <> eqsys



type
   EquationSystem s =
      EqGen.EquationSystem Env.Delta System.Node s
         (Term.Scalar SumProduct.Term (HSt.ScalarSymbol System.Node) (HSt.SignalSymbol System.Node))
         (Term.Signal SumProduct.Term (HSt.ScalarSymbol System.Node) (HSt.SignalSymbol System.Node))


-- @Henning -- please help here
-}


type
   EquationSystemNumeric s =
      EqGen.EquationSystem EqRecord.Absolute System.Node s
         (Stack (Var.Any System.Node) Double)
         (Stack (Var.Any System.Node) Double)

type DeltaResult = EqRecord.Delta (R.Result Double)


type Expression node s a v x = EqGen.Expression EqRecord.Absolute node s a v x


variable ::
   (Eq x, Arith.Sum x,
    EqGen.Element idx EqRecord.Absolute s a v
      ~ EqGen.VariableRecord EqRecord.Absolute s x,
    Env.AccessMap idx, Ord (idx node)) =>
   idx node -> Expression node s a v x
variable = EqGen.variable . Idx.absolute

infix 0 .==

(.==) ::
  (Eq x, Arith.Sum x,
   EqGen.Element idx EqRecord.Absolute s a v
      ~ EqGen.VariableRecord EqRecord.Absolute s x,
   Env.AccessMap idx, Ord (idx node)) =>
   idx node -> x ->
   EqGen.EquationSystem EqRecord.Absolute node s a v
evar .== val  =  variable evar EqGen.=.= EqGen.constant val


deltaPair ::
   (Ord (idx System.Node), Env.AccessMap idx,
    Var.Index idx, Var.Type idx ~ Var.Signal) =>
   (idx System.Node) -> Double -> Double -> EquationSystemNumeric s
deltaPair idx before delta =
   idx .== Stack.deltaPair (Var.Signal $ Var.index idx) before delta


difference sequenceFlowTopology env =
  EqGen.solve sequenceFlowTopology (makeGivenForDifferentialAnalysis env) 
        
                                      

makeGivenForDifferentialAnalysis ::
  Env.Complete System.Node DeltaResult DeltaResult ->
  EquationSystemNumeric s                         
makeGivenForDifferentialAnalysis (Env.Complete scal sig) = 
  (Idx.DTime Idx.initSection .== 1) <>
  (Idx.DTime sec2 .== 0) <>
  (Idx.Storage (Idx.SecNode Idx.initSection System.Battery) .== initStorage) <>
  (deltaPair (edgeVar Idx.Energy sec2 System.Tank System.ConBattery) 4 (-0.6)) <>
  (fold $ M.mapWithKey f $ Env.etaMap sig) <>
  mempty
  where f i rec =
           deltaPair i
              (checkDetermined "before" $ EqRecord.before rec)
              (checkDetermined "delta"  $ EqRecord.delta rec)

{-
makeGivenForDifferentialAnalysis env = (
  Idx.before (Idx.DTime Idx.initSection) .= 1)
  =<> (Idx.delta (Idx.DTime Idx.initSection) .= 0)
  =<> (Idx.before (Idx.Storage (Idx.SecNode Idx.initSection System.Battery)) .= 
      SumProduct.Atom (HSt.Symbol{HSt.index = Idx.before (Var.index $ Idx.Storage (Idx.SecNode Idx.initSection System.Battery)),
                              HSt.value = initStorage}))
  =<> (Idx.delta (Idx.Storage (Idx.SecNode Idx.initSection System.Battery)) .= 
      SumProduct.Atom (HSt.Symbol{HSt.index = Idx.delta (Var.index $ Idx.Storage (Idx.SecNode Idx.initSection System.Battery)),
                              HSt.value = 0}))
  =<> (Idx.before (Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia)) .= 
      SumProduct.Atom (HSt.Symbol{HSt.index = Idx.before (Var.index $ Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia)),
                                                                   HSt.value = 0}))
  =<> (Idx.delta (Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia)) .= 
     SumProduct.Atom (HSt.Symbol{HSt.index = Idx.delta (Var.index $ Idx.Storage (Idx.SecNode Idx.initSection System.VehicleInertia)),
                             HSt.value = 0}))
  =<> (fold $ concat $ map f (M.toList (Env.etaMap  $ Env.signal env)))
  =<> (fold $ concat $ map f (M.toList (Env.dtimeMap  $ Env.signal env)))
  =<> (fold $ concat $ map f (M.toList $ M.filterWithKey g $ Env.energyMap  $ Env.signal env))
  where
    f (i, x)  =  [(Idx.before i) .= SumProduct.Atom (HSt.Symbol{HSt.index = (Idx.before $ Var.index i), 
                                                            HSt.value =  (h $ Env.before x)}),
                  (Idx.delta i) .= SumProduct.Atom (HSt.Symbol{HSt.index = (Idx.delta $ Var.index i), 
                                                           HSt.value = (h $ Env.delta x)})]
    h (EqGen.Determined x) = x            

    g (Idx.Energy (Idx.SecNode _ x) (Idx.SecNode _ y)) _ =
       case (x,y) of
         (System.Resistance, System.Chassis) -> True
         (System.VehicleInertia, System.Chassis) -> True
         (System.RearBrakes, System.Chassis) -> True
         (System.FrontBrakes, System.ConFrontBrakes) -> True
         (System.ConES, System.ElectricSystem) -> True
         (System.Battery, System.ConBattery) -> True
         _ -> False
-}

