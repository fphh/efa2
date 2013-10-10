{-# LANGUAGE TypeOperators #-}
module EFA.Flow.Sequence.Record where

import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence as SeqFlowPlain
import qualified EFA.Flow.PartMap as PartMap
import EFA.Flow.PartMap (PartMap)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph
import EFA.Graph.Topology (Topology)
import EFA.Graph (Graph, DirEdge(DirEdge), unDirEdge)

import qualified EFA.Signal.Signal as Signal
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Record as Record
import EFA.Signal.Record (Record(Record), FlowRecord)
import EFA.Signal.Signal (fromScalar)
import EFA.Signal.Data (Data, Nil, (:>))
import EFA.Signal.Base (Sign(PSign, NSign, ZSign), BSum, DArith0)

import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified EFA.Utility.Map as MapU

import Control.Applicative (pure)

import qualified Data.Foldable as Fold
import qualified Data.Map as Map ; import Data.Map (Map)
import Data.Tuple.HT (mapPair, mapSnd)


{-
UnDirEdge must have Nothing as edge lebel
DirEdge must have Just as edge label
-}
type SignalTopology node v a =
        Graph node Graph.EitherEdge
           (Node.Type ())
           (Maybe (Flow (Signal.FFSignal v a)))

data Flow a = Flow {flowOut, flowIn :: a}

instance Functor Flow where
   fmap f (Flow o i) = Flow (f o) (f i)

type Section node v a =
        (Signal.TSignal v a, SignalTopology node v a)

flowTopologyFromRecord ::
   (Ord node, Show node,
    Fractional a, Ord a, BSum a, DArith0 a,
    SV.Walker v, SV.Storage v a) =>
   Topology node ->
   FlowRecord node v a ->
   Section node v a
flowTopologyFromRecord topo (Record time fs) =
   (,) time $
   Graph.fromMap (Graph.nodeLabels topo) $
   Map.unionsWith (error "flowTopologyFromRecord: duplicate edges") $
   Map.elems $
   Map.mapWithKey
      (\(DirEdge idx1 idx2) () ->
         let look = MapU.checkedLookup "Flow.flowTopologyFromRecord" fs
             normal   = look $ XIdx.ppos idx1 idx2
             opposite = look $ XIdx.ppos idx2 idx1
         in  case fromScalar $ Signal.sign $ Signal.sum normal of
                PSign ->
                   Map.singleton
                      (Graph.EDirEdge $ DirEdge idx1 idx2)
                      (Just $ Flow {flowOut = normal, flowIn = opposite})
                NSign ->
                   Map.singleton
                      (Graph.EDirEdge $ DirEdge idx2 idx1)
                      (Just $ Flow {flowOut = Signal.neg opposite, flowIn = Signal.neg normal})
                ZSign ->
                   Map.singleton
                      (Graph.EUnDirEdge $ unDirEdge idx1 idx2)
                      Nothing) $
   Graph.edgeLabels topo


fullFlow :: Flow (Result a) -> SeqFlow.Flow (Result a)
fullFlow flow =
   (pure Undetermined) {
      SeqFlow.flowEnergyOut = flowOut flow,
      SeqFlow.flowEnergyIn = flowIn flow
   }

fromGraphFromSequence ::
   (BSum a, SV.Zipper v, SV.Walker v, SV.Singleton v, SV.Storage v a,
    Node.C node) =>
   Sequ.List (Section node v a) ->
   SeqFlow.Graph node (Result (Data Nil a)) (Result (Data (v :> Nil) a))
fromGraphFromSequence sd =
   let sq = fmap (mapSnd SeqFlow.unknownTopologyNodes) sd
   in  SeqFlowPlain.Graph {
          SeqFlow.storages =
             fmap
                (storageMapFromList
                    (Fold.toList $ Sequ.mapWithSection const sq) .
                 storageEdges) $
             getStorageSequences $ fmap snd sq,
          SeqFlow.sequence =
             fmap (mapSnd (mapPair
                (Determined . Signal.unpack . Signal.delta,
                 Graph.mapEdge (fmap
                    (fullFlow . fmap (Determined . Signal.unpack)))))) $
             Sequ.toMap sq
       }


storageMapFromList ::
   (Ord edge,
    SeqFlow.Unknown a) =>
   [Idx.Section] ->
   [edge] ->
   (PartMap Idx.Section a, Map Idx.Boundary a, Map edge (SeqFlow.Carry a))
storageMapFromList secs =
   (,,)
      (PartMap.constant SeqFlow.unknown secs)
      (Map.fromList $ map (flip (,) SeqFlow.unknown . Idx.Following) $
       Idx.Init : map Idx.NoInit secs) .
   Map.fromListWith (error "duplicate storage edge") .
   map (flip (,) $ pure SeqFlow.unknown)

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

getStorageSequences ::
   (Node.C node) =>
   Sequ.List (Graph node Graph.EitherEdge (SeqFlow.Sums v) edgeLabel) ->
   Map node (Map Idx.Section (SeqFlow.Sums v))
getStorageSequences =
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
   fmap
      (mapSnd $ \(time, topo) ->
         Record.Record (Signal.TC time) $
         Map.unionsWith (error "envToPowerRecord: duplicate edges") $
         Map.elems $
         Map.mapWithKey
            (\e flow ->
               let se = Topo.structureEdgeFromDirEdge e
               in  Map.fromList $
                      (Idx.PPos se, Signal.TC $ SeqFlow.flowPowerOut flow) :
                      (Idx.PPos $ Idx.flip se, Signal.TC $ SeqFlow.flowPowerIn flow) :
                      []) $
         Graph.edgeLabels $ SeqFlow.dirFromFlowGraph topo) .
   SeqFlow.sequence
