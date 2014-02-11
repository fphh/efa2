{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Modules.Optimisation.NonIO where

import qualified Modules.Optimisation as Optimisation
import qualified Modules.Optimisation.Base as Base
import qualified Modules.Utility as ModUt
import qualified Modules.Setting as ModSet
import Modules.Optimisation (external)

import qualified EFA.Application.ReqsAndDofs as ReqsAndDofs
import qualified EFA.Application.Type as Type
import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Simulation as AppSim
import EFA.Application.Sweep (Sweep)

import qualified EFA.Graph.Topology.Node as Node
--import qualified EFA.Graph.Topology as Topology

import qualified EFA.Flow.Topology.Record as TopoRecord
import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Flow.Topology.Index as TopoIdx
--import qualified EFA.Flow.Topology as FlowTopo

import qualified EFA.Flow.Sequence.Absolute as SeqAbs
import qualified EFA.Flow.Sequence.Quantity as SeqQty
import qualified EFA.Flow.Sequence.Record as SeqRec
import qualified EFA.Flow.Sequence.Index as SeqIdx

import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.State.Absolute as StateEqAbs
--import qualified EFA.Flow.State as FlowState

import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Chop as Chop
import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Vector as SV

import EFA.Signal.Data (Data(Data), Nil, (:>))


import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified Data.Map as Map; import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as UV
import Data.Monoid (Monoid, mempty, (<>))


quasiStationaryOptimisation ::
  (Ord a, Ord (sweep UV.Vector a), UV.Unbox a,
   Monoid (sweep UV.Vector Bool),
   Arith.Product (sweep UV.Vector a),Show a,
   Arith.Constant a,
   Node.C node, Show node,
   Sweep.SweepVector UV.Vector Bool,
   Sweep.SweepVector UV.Vector a,
   Sweep.SweepMap sweep UV.Vector a Bool,
   Sweep.SweepMap sweep UV.Vector a a,
   Sweep.SweepClass sweep UV.Vector Bool,
   Sweep.SweepClass sweep UV.Vector a) =>
  One.OptimalEnvParams node [] sweep UV.Vector sigVec a ->
  Map node (One.SocDrive a) -> 
  Map Idx.State (One.StateForcing a) ->
  Map Idx.State (Map [a] (Type.PerStateSweep node sweep UV.Vector a)) ->
  Type.QuasiStationary node sweep UV.Vector a

quasiStationaryOptimisation params stoForcings stateForcings perStateSweep =
  let a = Base.optimalObjectivePerState params stoForcings perStateSweep
      b = Base.expectedValuePerState perStateSweep
      c = Base.selectOptimalState stateForcings a
  in Type.QuasiStationary perStateSweep a b c


simulation :: forall a node sigVec sweep list vec .
   ( Arith.ZeroTestable a, UV.Unbox a,SV.Storage vec a, 
     SV.Convert vec UV.Vector,SV.Convert sigVec UV.Vector,
    Ord a,Fractional a,
    Show a,
    Arith.Constant a,SV.Convert sigVec sigVec,
   Node.C node, Show node, Ord node,Show (sigVec a),SV.Len (sigVec a),SV.Zipper sigVec,
   SV.Walker sigVec,SV.Convert sigVec [],Eq (sigVec a), SV.Lookup sigVec, SV.Find sigVec,
   SV.Storage sigVec Bool,
   SV.Storage sigVec a,
   SV.Singleton sigVec,
   SV.FromList sigVec,
   Num a, 
   Arith.ZeroTestable (sweep vec a),
   Arith.Product (sweep vec a),
   Arith.Sum (sweep vec a),
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a,
   Sweep.SweepMap sweep vec a a) =>
  One.OptimalEnvParams node list sweep vec sigVec a ->
  Map (TopoIdx.Position node) (Sig.PSignal2 Vector sigVec a) ->
  Record.PowerRecord node sigVec a ->
  Type.Simulation node sweep vec sigVec a

simulation params dofsMatrices reqsRec =
  let (prest, plocal) =
        case map (Record.getSig reqsRec) (ReqsAndDofs.unReqs $ One.reqsPos params) of
             [r, l] -> (r, l)
             _ -> error "NonIO.simulation: number of signals"


      dofsSignals :: Map (TopoIdx.Position node) (Sig.PSignal sigVec a)
      dofsSignals =  Map.mapWithKey f dofsMatrices
        where f key mat = Sig.tzipWith (Sig.interp2WingProfile ("simulation-interpolate Signals" ++ show key) 
                               (One.varReqRoomPower1D params) (One.varReqRoomPower2D params) $ Sig.convert mat) prest plocal         
                         --      (Sig.convert ModSet.varRestPower1D) (Sig.convert ModSet.varLocalPower) mat) prest plocal
{-        for dofsMatrices $ \mat ->
          Sig.tzipWith
            (Sig.interp2WingProfile "simulation-interolateSignals"
              ModSet.varRestPower1D ModSet.varLocalPower mat)
            prest plocal -}

      givenSigs = Record.addSignals (Map.toList dofsSignals) reqsRec


      envSims :: TopoQty.Section node (Result (Data (sigVec :> Nil) a))
      envSims = 
        AppSim.solve
          (One.systemTopology params)
          (One.etaAssignMap params)
          (One.etaMap params)
          givenSigs

      -- Liefert leider nur delta-Zeiten und keinen Zeitvektor
      -- Deshalb merken wir uns den urspruenglichen Zeitvektor
      recZeroCross = 
        Chop.addZeroCrossings $ Base.convertRecord $
          (Base.envToPowerRecord envSims)
            { Record.recordTime = Record.recordTime givenSigs }


--      sequencePowers :: Sequ.List (Record.PowerRecord ModSet.Node [] a)
      sequencePowers = Chop.genSequ recZeroCross

      (_, sequenceFlowsFilt) =
        Sequ.unzip $
        Sequ.filter (Record.major (Sig.toScalar 0) (Sig.toScalar 0.2) . snd) $
        fmap (\x -> (x, Record.partIntegrate x)) sequencePowers



      sequenceFlowGraphSim ::
        SeqQty.Graph node
          (Result (Data Nil a)) (Result (Data ([] :> Nil) a))
          
      sequenceFlowGraphSim =
        SeqAbs.solveOpts
          (SeqAbs.independentInOutSums SeqAbs.optionsDefault)
          (SeqRec.flowGraphFromSequence $
            fmap (TopoRecord.flowTopologyFromRecord (One.systemTopology params)) $
            sequenceFlowsFilt)
          (Map.foldWithKey
            (\st val -> ((SeqIdx.storage Idx.initial st SeqAbs..= Data val) <>))
            mempty (One.unInitStorageSeq $ One.initStorageSeq params))

{-
      renumberStates g = g { FlowState.states = m }
        where m = Map.fromList $ map f $ Map.toList $ StateQty.states g
              num = fromIntegral
                    . Topology.flowNumber (One.systemTopology params)
                    . FlowTopo.topology
              f (idx, section) =
                (let st = Idx.State (num section) in trace (show idx ++ " -> 
                " ++ show st) st, section)

-}

      stateFlowGraphSim :: Type.EnvResult node (Data Nil a)
      stateFlowGraphSim = -- renumberStates $
        StateEqAbs.solveOpts
          Optimisation.optionsScalar
           (StateQty.graphFromCumResult $
           StateQty.fromSequenceFlowResult False $
           SeqQty.mapGraph id (fmap Arith.integrate) $
           external (One.initStorageState params) sequenceFlowGraphSim)
          mempty

      stateFlowGraphSweep :: Type.EnvResult node (sweep vec a)
      stateFlowGraphSweep = -- renumberStates $
        StateEqAbs.solveOpts
          Optimisation.options
          (toSweep params $ 
           StateQty.graphFromCumResult $
           StateQty.fromSequenceFlowResult False $
           SeqQty.mapGraph id (fmap Arith.integrate) $
           external (One.initStorageState params) sequenceFlowGraphSim)
          mempty



  in Type.Simulation stateFlowGraphSim stateFlowGraphSweep 
     sequenceFlowGraphSim givenSigs recZeroCross


toSweep ::
  (Sweep.SweepClass sweep vec a, Arith.Constant a) =>
  One.OptimalEnvParams node f sweep vec sigVec a ->
  StateQty.Graph node (Result (Data Nil a)) (Result (Data Nil a)) ->
  StateQty.Graph node (Result (sweep vec a)) (Result (sweep vec a))
toSweep params = StateQty.mapGraph f f
  where one = Sweep.fromRational (One.sweepLength params) Arith.one
        f (Determined (Data x)) = Determined $ Sweep.replicate one x
        f Undetermined = Undetermined


optimiseAndSimulate ::
  (SV.Storage sigVec (Maybe (Result a)),
   Eq (sigVec a),
                      SV.Lookup sigVec,
                      SV.Find sigVec,
                      SV.Convert sigVec sigVec,
   Show (sigVec a),
   SV.Zipper sigVec,
   SV.Walker sigVec,
   SV.Storage sigVec a,
   SV.Storage sigVec Bool,
   SV.Singleton sigVec,
   SV.Len (sigVec a),
   SV.Convert sigVec [],
   SV.Convert sigVec UV.Vector,
   SV.FromList sigVec, 
   Fractional a, Arith.ZeroTestable a,
    Show node, Node.C node,Ord a, 
    Show a, UV.Unbox a, 
    Arith.Constant a,Arith.Product a,Arith.Sum a,
   Ord (Sweep UV.Vector a),
   Monoid (Sweep UV.Vector Bool),
   Show (UV.Vector a),
   Arith.ZeroTestable (Sweep UV.Vector a),
   Arith.Product (Sweep UV.Vector a),
   Sweep.SweepVector UV.Vector a,
   Sweep.SweepVector UV.Vector Bool,
   Sweep.SweepMap Sweep UV.Vector a a,
   Sweep.SweepMap Sweep UV.Vector a Bool,
   Sweep.SweepClass Sweep UV.Vector a,
   Sweep.SweepClass Sweep UV.Vector Bool) => 
  One.OptimalEnvParams node [] Sweep UV.Vector sigVec a ->
  Map node (One.SocDrive a) -> 
  Map Idx.State (One.StateForcing a) ->
  Record.PowerRecord node sigVec a ->
  Map Idx.State (Map [a] (Type.PerStateSweep node Sweep UV.Vector a)) ->
  Type.Optimisation node Sweep UV.Vector sigVec a
  
optimiseAndSimulate params stoForcing stateForcing reqsRec perStateSweep =
  let optimalResult = quasiStationaryOptimisation params stoForcing stateForcing perStateSweep

      dofsMatrices = 
        Map.map (Sig.map ModUt.nothing2Nan) $
          Base.signCorrectedOptimalPowerMatrices
            params
            (Type.optimalState optimalResult)
            (One.dofsPos params)

  in Type.Optimisation
       optimalResult
       (simulation params dofsMatrices reqsRec)


optimiseStatesAndSimulate :: (Ord a, Show a, Arith.Sum a,
                              Arith.Constant a,Ord node,
                              Eq (sigVec a),
                      SV.Lookup sigVec,
                      SV.Find sigVec,
                      SV.Convert sigVec sigVec,
                      Show node,
                      SV.Storage sigVec (Maybe (Result a)),
                      SV.FromList sigVec,Fractional a,
                      Show (sigVec a),
                      UV.Unbox a,
                      SV.Zipper sigVec,
                      SV.Walker sigVec,
                      SV.Storage sigVec a,
                      SV.Storage sigVec Bool,
                      SV.Storage vec a,
                      SV.Singleton sigVec,
                      SV.Len (sigVec a),
                      SV.Convert sigVec [],
                      SV.Convert sigVec UV.Vector,
                      SV.Convert vec UV.Vector,
                      Node.C node,
                      Arith.ZeroTestable (sweep vec a),
                      Arith.ZeroTestable a,
                      Arith.Product (sweep vec a),
                      Sweep.SweepVector vec a,
                      Sweep.SweepMap sweep vec a a,
                      Sweep.SweepClass sweep vec a)=>
                             One.OptimalEnvParams node [] sweep vec sigVec a ->
                             Map Idx.State (One.StateForcing a) ->
                             Record.PowerRecord node sigVec a ->
                             Map Idx.State (Map [a] (Maybe (a, a, Type.EnvResult node a)))->
                             (Map [a](Maybe (a, a, Idx.State, Type.EnvResult node a)),
                                 Type.Simulation node sweep vec sigVec a)
                             
optimiseStatesAndSimulate params stateForcing reqsRec optStates = 
  let optimalState = Base.selectOptimalState stateForcing optStates
  
      dofsMatrices =
        Map.map (Sig.map ModUt.nothing2Nan) $
          Base.signCorrectedOptimalPowerMatrices
            params
            optimalState
            (One.dofsPos params)

  in (optimalState, simulation params dofsMatrices reqsRec)
  