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
import qualified Data.Vector.Unboxed as UV(Unbox)
import Data.Monoid (Monoid, mempty, (<>))


quasiStationaryOptimisation ::
  (Ord a, Ord (sweep vec a), UV.Unbox a,
   SV.Storage vec (Bool, a, a),
   SV.Storage vec (a, a),
   SV.Zipper vec,
   SV.Walker vec,
   SV.Storage vec (Bool, a),
   SV.Storage vec a,
   SV.Storage vec Bool,
   Monoid (sweep vec Bool),
   Arith.Product (sweep vec a),Show a,
   Arith.Constant a,
   Node.C node, Show node,
   Sweep.SweepVector vec Bool,
   Sweep.SweepVector vec a,
   Sweep.SweepMap sweep vec a Bool,
   Sweep.SweepMap sweep vec a a,
   Sweep.SweepClass sweep vec Bool,
   Sweep.SweepClass sweep vec a) =>
  One.OptimalEnvParams node [] sweep vec vec a ->
  Map node (One.SocDrive a) -> 
  Map Idx.State (One.StateForcing a) ->
  Map Idx.State (Map [a] (Type.PerStateSweep node sweep vec a)) ->
  Type.QuasiStationary node sweep vec a
quasiStationaryOptimisation params stoForcings stateForcings perStateSweep =
  let a = Base.optimalObjectivePerState params stoForcings perStateSweep
      b = Base.expectedValuePerState perStateSweep
      c = Base.selectOptimalState stateForcings a
  in Type.QuasiStationary perStateSweep a b c

simulation ::
  forall node sweep vec vec2 list a .
   (vec ~ Vector, Eq (vec a), Arith.ZeroTestable a, 
    Ord a,Fractional a,
    Show a,
    Show (vec a),
    SV.Zipper vec,
    SV.Walker vec,
    SV.Storage vec Bool,
    SV.Storage vec a,
    SV.Singleton vec,
    SV.Lookup vec,
    SV.Len (vec a),
    SV.Find vec,
    Arith.Constant a,
   Node.C node, Show node, Ord node,Show (vec2 a),SV.Len (vec2 a),SV.Zipper vec2,
   SV.Walker vec2,SV.Convert vec2 [],
   Num a, 
--   SV.Transpose [] vec, SV.Storage vec [a],
   SV.Storage vec2 Bool,UV.Unbox a,
   SV.Storage vec2 a,
   SV.Singleton vec2,
   SV.FromList vec2,
   Arith.ZeroTestable (sweep vec a),
   Arith.Product (sweep vec a),
   Arith.Sum (sweep vec a),
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a,
   Sweep.SweepMap sweep vec a a) =>
  One.OptimalEnvParams node list sweep vec vec2 a ->
  Map (TopoIdx.Position node) (Sig.PSignal2 Vector vec a) ->
  Record.PowerRecord node vec2 a ->
  Type.Simulation node sweep vec vec2 a
simulation params dofsMatrices reqsRec =
  let (prest, plocal) =
        case map (Record.getSig reqsRec) (ReqsAndDofs.unReqs $ One.reqsPos params) of
             [r, l] -> (r, l)
             _ -> error "NonIO.simulation: number of signals"


      dofsSignals :: Map (TopoIdx.Position node) (Sig.PSignal vec2 a)
      dofsSignals =  Map.mapWithKey f dofsMatrices
        where f key mat = Sig.tzipWith (Sig.interp2WingProfile ("simulation-interpolate Signals" ++ show key) 
                               (One.varReqRoomPower1D params) (One.varReqRoomPower2D params) mat) prest plocal         
                         --      (Sig.convert ModSet.varRestPower1D) (Sig.convert ModSet.varLocalPower) mat) prest plocal
{-        for dofsMatrices $ \mat ->
          Sig.tzipWith
            (Sig.interp2WingProfile "simulation-interolateSignals"
              ModSet.varRestPower1D ModSet.varLocalPower mat)
            prest plocal -}

      givenSigs = Record.addSignals (Map.toList dofsSignals) reqsRec


      envSims :: TopoQty.Section node (Result (Data (vec2 :> Nil) a))
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


      -- sequencePowers :: Sequ.List (PowerRecord System.Node [] a)
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
  One.OptimalEnvParams node f sweep vec vec2 a ->
  StateQty.Graph node (Result (Data Nil a)) (Result (Data Nil a)) ->
  StateQty.Graph node (Result (sweep vec a)) (Result (sweep vec a))
toSweep params = StateQty.mapGraph f f
  where one = Sweep.fromRational (One.sweepLength params) Arith.one
        f (Determined (Data x)) = Determined $ Sweep.replicate one x
        f Undetermined = Undetermined


optimiseAndSimulate ::
  (vec ~ Vector,Fractional a, Arith.ZeroTestable a,
    Show node, Node.C node,Ord a, 
    Show a, UV.Unbox a, 
    Arith.Constant a,Arith.Product a,Arith.Sum a,
   Ord (Sweep vec a),
   Monoid (Sweep vec Bool),
   SV.Storage vec (vec (Maybe (Result a))),
   SV.Storage vec (Maybe (Result a)),
   SV.FromList vec,
   Show (vec a),
   SV.Singleton vec,
   SV.Len (vec a),
   SV.Convert vec [],
   Arith.ZeroTestable (Sweep vec a),
   SV.Zipper vec,
   SV.Walker vec,
   SV.Storage vec Bool,
   SV.Storage vec a,
   SV.Storage vec (Bool, a),
   SV.Storage vec (a, a),
   SV.Storage vec (Bool, a, a),
   Arith.Product (Sweep vec a),
   Sweep.SweepVector vec a,
   SV.Storage vec (vec a),
   Sweep.SweepVector vec Bool,
   Sweep.SweepMap Sweep vec a a,
   Sweep.SweepMap Sweep vec a Bool,
   Sweep.SweepClass Sweep vec a,
   Sweep.SweepClass Sweep vec Bool) =>
  One.OptimalEnvParams node [] Sweep vec vec a ->
  Map node (One.SocDrive a) -> 
  Map Idx.State (One.StateForcing a) ->
  Record.PowerRecord node vec a ->
  Map Idx.State (Map [a] (Type.PerStateSweep node Sweep vec a)) ->
  Type.Optimisation node Sweep vec vec a
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

optimiseStatesAndSimulate :: (vec ~ Vector,Fractional a, UV.Unbox a, Arith.ZeroTestable a,
                              Show node, Arith.Product (sweep vec a),Arith.Constant a,
                              SV.Walker vec,Ord a, Show a, Arith.Sum a,
                              SV.Storage vec (vec a),
                              SV.Storage vec (vec (Maybe (Result a))),
                              SV.Storage vec (Maybe (Result a)),
                              SV.FromList vec,
                              SV.Storage vec a,
                              Node.C node, Arith.ZeroTestable (sweep vec a),
                              Sweep.SweepVector vec a,
                              Sweep.SweepClass sweep vec a,
                              Sweep.SweepMap sweep vec a a,
                              Show (vec2 a),
                              SV.Zipper vec2,
                              SV.Walker vec2,
                              SV.Storage vec2 a,
                              SV.Storage vec2 Bool,
                              SV.Singleton vec2,
                              SV.Len (vec2 a),
                              SV.FromList vec2,
                              SV.Convert vec2 []) =>
                             One.OptimalEnvParams node [] sweep vec vec2 a ->
                             Map Idx.State (One.StateForcing a) ->
                             Record.PowerRecord node vec2 a ->
                             Map Idx.State (Map [a] (Maybe (a, a, Type.EnvResult node a)))->
                             (Map [a](Maybe (a, a, Idx.State, Type.EnvResult node a)),
                                 Type.Simulation node sweep vec vec2 a)
optimiseStatesAndSimulate params stateForcing reqsRec optStates = 
  let optimalState = Base.selectOptimalState stateForcing optStates
  
      dofsMatrices =
        Map.map (Sig.map ModUt.nothing2Nan) $
          Base.signCorrectedOptimalPowerMatrices
            params
            optimalState
            (One.dofsPos params)

  in (optimalState, simulation params dofsMatrices reqsRec)
  