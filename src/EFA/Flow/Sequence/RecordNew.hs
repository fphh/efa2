{-# LANGUAGE TypeOperators #-}
module EFA.Flow.Sequence.RecordNew (
   flowGraphFromSequence,
   flowGraphToPowerRecords,
   ) where

import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Topology.Record as TopoRecord
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology as FlowTopoPlain

import qualified EFA.Graph.Topology.Node as Node

-- New Imports
import qualified EFA.Flow.Topology.RecordNew as TopoRecordNew

import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Record as Record
import EFA.Signal.Data (Data, Nil, (:>))

import qualified  EFA.Data.OD.Signal.Flow as SignalFlow
import qualified EFA.Data.Sequence as DataSequ
import qualified EFA.Data.Vector as DV

import EFA.Equation.Result (Result)
import EFA.Equation.Arithmetic (Sum)

import Data.Tuple.HT (mapSnd)

{-
flowGraphFromSequenceNewSig ::
   (Sum a,
    Node.C node) =>
   DataSequ.List (TopoRecord.Section node v a) ->
   SeqFlow.Graph node (Result a) (Result (SignalFlow.Data inst vec a))
flowGraphFromSequenceNewSig =
   SeqFlow.graphFromSections .
   fmap
      (\(FlowTopoPlain.Section lab topo) ->
         TopoRecordNew.fromSectionNewSig $
         FlowTopoPlain.Section lab $
         FlowTopo.unknownTopologyNodes topo) -}

flowGraphFromSequence ::
   (Sum a, SV.Zipper v, SV.Walker v, SV.Singleton v, SV.Storage v a,
    Node.C node) =>
   Sequ.List (TopoRecord.Section node v a) ->
   SeqFlow.Graph node (Result (Data Nil a)) (Result (Data (v :> Nil) a))
flowGraphFromSequence =
   SeqFlow.graphFromSections .
   fmap
      (\(FlowTopoPlain.Section lab topo) ->
         TopoRecord.fromSection $
         FlowTopoPlain.Section lab $
         FlowTopo.unknownTopologyNodes topo)


flowGraphToPowerRecords ::
   (Ord node) =>
   SeqFlow.Graph node a0 (Data (v :> Nil) a) ->
   Sequ.Map (Record.PowerRecord node v a)
flowGraphToPowerRecords =
   fmap (mapSnd TopoRecord.sectionToPowerRecord) .
   SeqFlow.sequence
