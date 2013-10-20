{-# LANGUAGE TypeOperators #-}
module EFA.Flow.Sequence.Record (
   flowGraphFromSequence,
   flowGraphToPowerRecords,
   ) where

import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence as SeqFlowPlain
import qualified EFA.Flow.Topology.Record as TopoRecord
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Flow.Storage.Quantity as Storage

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Record as Record
import EFA.Signal.Data (Data, Nil, (:>))
import EFA.Signal.Base (BSum)

import EFA.Equation.Result (Result)

import qualified Data.Foldable as Fold
import Data.Tuple.HT (mapSnd)


flowGraphFromSequence ::
   (BSum a, SV.Zipper v, SV.Walker v, SV.Singleton v, SV.Storage v a,
    Node.C node) =>
   Sequ.List (TopoRecord.Section node v a) ->
   SeqFlow.Graph node (Result (Data Nil a)) (Result (Data (v :> Nil) a))
flowGraphFromSequence sd =
   let sq =
          fmap
             (\(FlowTopoPlain.Section lab topo) ->
                FlowTopoPlain.Section lab $
                FlowTopo.unknownTopologyNodes topo) sd
   in  SeqFlowPlain.Graph {
          SeqFlow.storages =
             fmap
                (SeqFlow.storageMapFromList
                    (Fold.toList $ Sequ.mapWithSection const sq) .
                 Storage.forwardEdgesFromSums) $
             SeqFlow.storageSequences $ fmap FlowTopo.topology sq,
          SeqFlow.sequence =
             fmap (mapSnd TopoRecord.fromSection) $ Sequ.toMap sq
       }


flowGraphToPowerRecords ::
   (Ord node) =>
   SeqFlow.Graph node a0 (Data (v :> Nil) a) ->
   Sequ.Map (Record.PowerRecord node v a)
flowGraphToPowerRecords =
   fmap (mapSnd TopoRecord.sectionToPowerRecord) .
   SeqFlow.sequence
