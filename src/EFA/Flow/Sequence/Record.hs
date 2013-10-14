{-# LANGUAGE TypeOperators #-}
module EFA.Flow.Sequence.Record (
   fromGraphFromSequence,
   flowGraphToPowerRecords,
   ) where

import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence as SeqFlowPlain
import qualified EFA.Flow.Topology.Record as TopoRecord
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Flow.PartMap as PartMap
import EFA.Flow.StorageGraph (StorageGraph(StorageGraph))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph
import EFA.Graph (Graph)

import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Record as Record
import EFA.Signal.Data (Data, Nil, (:>))
import EFA.Signal.Base (BSum)

import EFA.Equation.Result (Result)

import Control.Applicative (pure)

import qualified Data.Foldable as Fold
import qualified Data.Map as Map ; import Data.Map (Map)
import Data.Tuple.HT (mapSnd)


fromGraphFromSequence ::
   (BSum a, SV.Zipper v, SV.Walker v, SV.Singleton v, SV.Storage v a,
    Node.C node) =>
   Sequ.List (TopoRecord.Section node v a) ->
   SeqFlow.Graph node (Result (Data Nil a)) (Result (Data (v :> Nil) a))
fromGraphFromSequence sd =
   let sq =
          fmap
             (\(FlowTopoPlain.Section lab topo) ->
                FlowTopoPlain.Section lab $
                FlowTopo.unknownTopologyNodes topo) sd
   in  SeqFlowPlain.Graph {
          SeqFlow.storages =
             fmap
                (storageMapFromList
                    (Fold.toList $ Sequ.mapWithSection const sq) .
                 storageEdges) $
             storageSequences $ fmap FlowTopo.topology sq,
          SeqFlow.sequence =
             fmap (mapSnd TopoRecord.fromSection) $ Sequ.toMap sq
       }

storageMapFromList ::
   (Ord node, SeqFlow.Unknown a) =>
   [Idx.Section] ->
   [XIdx.StorageEdge node] ->
   (StorageGraph Idx.Section node a (SeqFlow.Carry a), Map Idx.Boundary a)
storageMapFromList secs edges =
   (StorageGraph
      (PartMap.constant SeqFlow.unknown secs)
      (Map.fromListWith (error "duplicate storage edge") $
       map (flip (,) (pure SeqFlow.unknown)) edges),
    Map.fromList $ map (flip (,) SeqFlow.unknown . Idx.Following) $
    Idx.Init : map Idx.NoInit secs)

storageEdges ::
   Map Idx.Section (SeqFlow.Sums v) -> [Idx.StorageEdge Idx.Section node]
storageEdges stores = do
   let ins  = Map.mapMaybe SeqFlow.sumIn stores
   let outs = Map.mapMaybe SeqFlow.sumOut stores
   secin <- Idx.Init : map Idx.NoInit (Map.keys ins)
   secout <-
      (++[Idx.Exit]) $ map Idx.NoExit $ Map.keys $
      case secin of
         Idx.Init -> outs
         Idx.NoInit s -> snd $ Map.split s outs
   return $ Idx.StorageEdge secin secout

storageSequences ::
   (Node.C node) =>
   Sequ.List (Graph node Graph.EitherEdge (SeqFlow.Sums v) edgeLabel) ->
   Map node (Map Idx.Section (SeqFlow.Sums v))
storageSequences =
   Map.unionsWith (Map.unionWith (error "duplicate section for node"))
   .
   Fold.toList
   .
   Sequ.mapWithSection
      (\s g ->
         fmap (Map.singleton s) $
         Map.filterWithKey (const . Topo.isStorage . Node.typ) $
         Graph.nodeLabels g)


flowGraphToPowerRecords ::
   (Ord node) =>
   SeqFlow.Graph node a0 (Data (v :> Nil) a) ->
   Sequ.Map (Record.PowerRecord node v a)
flowGraphToPowerRecords =
   fmap (mapSnd TopoRecord.sectionToPowerRecord) .
   SeqFlow.sequence
