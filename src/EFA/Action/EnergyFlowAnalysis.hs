{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module EFA.Action.EnergyFlowAnalysis where

--import qualified EFA.Data.Sequence as DataSequ
--import qualified EFA.Flow.Topology.Index as TopoIdx
--import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Action.Flow.Topology.State as TopoState
--import qualified EFA.Utility.Map as MapU
import qualified EFA.Graph.Topology as Topo
--import EFA.Graph (DirEdge(DirEdge), unDirEdge)
--import EFA.Flow.Topology.Record (Flow(Flow),flowOut,flowIn)
import qualified EFA.Application.Optimisation.Optimisation as Optimisation
--import qualified EFA.Application.Optimisation.Params as Params
--import qualified EFA.Application.Optimisation.Balance as Balance
--import qualified EFA.Application.Optimisation.Base as Base
-- import EFA.Application.Optimisation.Optimisation (external)
import qualified EFA.Action.Utility as ActUt

--import qualified EFA.Application.Optimisation.ReqsAndDofs as ReqsAndDofs
--import qualified EFA.Application.Type as Type
--import qualified EFA.Application.Optimisation.Sweep as Sweep
--import qualified EFA.Application.Optimisation.Balance as Forcing
--import qualified EFA.Application.Simulation as AppSim
--import qualified EFA.Data.Axis.Strict as Strict
--import qualified EFA.Data.OD.Signal.Chop as Chop
--import qualified EFA.Data.OD.Signal.Flow as SignalFlow
--import qualified EFA.Data.Vector as DV
import qualified EFA.Graph.Topology.Node as Node
--import qualified EFA.Equation.Verify as Verify
--import qualified EFA.Flow.Sequence.Algorithm as SeqAlgo

--import qualified EFA.Graph as Graph

import qualified EFA.Flow.Topology.Record as TopoRecord

import qualified EFA.Flow.Sequence.Absolute as SeqAbs
import qualified EFA.Flow.Sequence.Quantity as SeqQty
import qualified EFA.Flow.Sequence.Record as SeqRec
import qualified EFA.Flow.Sequence.Index as SeqIdx
--import qualified EFA.Flow.SequenceState.EquationSystem as SeqStateEqSys

--import qualified EFA.Flow.State.Absolute as StateAbs

import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.State.Absolute as StateEqAbs
--import qualified EFA.Flow.Storage.EquationSystem as StorageEqSys

import qualified EFA.Flow.SequenceState.Index as Idx

-- New Imports
--import qualified EFA.Flow.Sequence.RecordNew as SeqRecNew
--import qualified EFA.Flow.Sequence.QuantityNew as SeqQtyNew
--import qualified EFA.Flow.Topology.RecordNew as TopoRecordNew

import qualified EFA.Data.Interpolation as Interp

--import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Chop as Chop
import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Vector as SV

--import EFA.Utility.List (vhead)

import EFA.Signal.Data (Data(Data), Nil,(:>))

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result)

import qualified Data.Map as Map
--import qualified Data.Vector.Unboxed as UV
import Data.Monoid (Monoid, mempty, (<>))
   
--import qualified EFA.Flow.State.Absolute as StateAbs
--import qualified EFA.Flow.Storage.EquationSystem as StorageEqSys

-- import qualified EFA.Signal.Vector as Vec
                                      
import EFA.Utility(Caller,
--                   merror,(|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "EnergyFlowAnalysis"

nc :: FunctionName -> Caller
nc = genCaller modul

type EnvResult node a = StateQty.Graph node (Result a) (Result a)

newtype InitStorageState node a =
  InitStorageState { unInitStorageState :: Map.Map node a } deriving Show

newtype InitStorageSeq node a =
  InitStorageSeq { unInitStorageSeq :: Map.Map node a } deriving Show


data EFAParams node a = EFAParams 
                 {accessInitStorageState :: InitStorageState node a,
                  accessInitStorageSeq :: InitStorageSeq node a, 
                  accessZeroDetectionEps :: a}
  

data EnergyFlowAnalysis node inst sigVec a = EnergyFlowAnalysis {
    accessSeqFlowRecord :: Sequ.List (Record.FlowRecord node sigVec (Interp.Val a)),
    accessSeqFlowGraph ::
      SeqQty.Graph node (Result (Data Nil (Interp.Val a))) (Result (Data (sigVec :> Nil) (Interp.Val a))),
    accessStateFlowGraph :: EnvResult node (Data Nil (Interp.Val a))}


energyFlowAnalysisOld ::
  (Ord a, Show node, SV.Convert [] sigVec,
   Eq (sigVec a), SV.Storage sigVec Bool,
   SV.Storage sigVec a, SV.Convert sigVec [],
   SV.Walker sigVec, SV.Singleton sigVec,
   SV.Zipper sigVec,
   SV.Storage sigVec (Interp.Val a),
   Show a, Node.C node,
   Arith.ZeroTestable (Interp.Val a),
   Show (sigVec a),
   Eq (sigVec (Interp.Val a)),
   Arith.Constant a) =>
  Topo.Topology node ->
  EFAParams node (Interp.Val a) ->
  Sequ.List (Record.FlowRecord node sigVec (Interp.Val a)) ->
  EnergyFlowAnalysis node inst sigVec a
energyFlowAnalysisOld topology efaParams sequenceFlowsFilt = 
    let 
      (InitStorageSeq initStorageSeq) = accessInitStorageSeq efaParams 
      initStorageState = accessInitStorageState efaParams
      sequenceFlowGraph =
        SeqAbs.solveOpts
          (SeqAbs.independentInOutSums SeqAbs.optionsDefault)
          (SeqRec.flowGraphFromSequence $
            fmap (TopoRecord.flowTopologyFromRecord topology) $
            sequenceFlowsFilt)
          (Map.foldWithKey
            (\st val -> ((SeqIdx.storage Idx.initial st SeqAbs..= Data val) <>))
            mempty initStorageSeq)

      stateFlowGraph = TopoState.absoluteStateFlowGraph topology $
        StateEqAbs.solveOpts
          Optimisation.optionsScalar
           (StateQty.graphFromCumResult $
           StateQty.fromSequenceFlowResult False $
           SeqQty.mapGraph id id $
           external initStorageState sequenceFlowGraph)
          mempty
  in EnergyFlowAnalysis sequenceFlowsFilt sequenceFlowGraph stateFlowGraph

external ::
  (Eq (v a), Arith.Constant a, SV.Zipper v,
   SV.Storage v Bool, Arith.ZeroTestable a,
  SV.Walker v, SV.Singleton v, SV.Storage v a, Node.C node, Show node) =>
  InitStorageState node a ->
  SeqQty.Graph node (Result (Data Nil a)) (Result (Data (v :> Nil) a)) ->
  SeqQty.Graph node (Result (Data Nil a)) (Result (Data (v :> Nil) a))
external (InitStorageState initSto) flowGraph =
  SeqAbs.solveOpts (SeqAbs.independentInOutSums SeqAbs.optionsDefault) flowGraph $
  Map.foldWithKey f mempty initSto
  where f st val = ((SeqIdx.storage Idx.initial st SeqAbs..= Data val) <>)

