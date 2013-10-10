module EFA.Flow.Sequence where

import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Topology as FlowTopo
import qualified EFA.Flow.StorageGraph as StorageGraph
import qualified EFA.Flow.PartMap as PartMap
import EFA.Flow.StorageGraph (StorageGraph(StorageGraph))

import qualified EFA.Graph.Flow as Flow

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph
import EFA.Graph.Topology (FlowTopology)

import qualified EFA.Signal.Sequence as Sequ

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Foldable as Fold
import qualified Data.List.HT as ListHT

import Data.Tuple.HT (mapPair)
import Data.Maybe (mapMaybe)

import Prelude hiding (sequence)


type
   Storages node storageLabel boundaryLabel storageEdgeLabel =
      Map node
         (StorageGraph Idx.Section node storageLabel storageEdgeLabel,
          Map Idx.Boundary boundaryLabel)

type
   Sequence node structEdge sectionLabel nodeLabel structLabel =
      Sequ.Map
         (FlowTopo.Section node structEdge sectionLabel nodeLabel structLabel)

data
   Graph node structEdge
         sectionLabel nodeLabel storageLabel boundaryLabel
         structLabel storageEdgeLabel =
      Graph {
         storages :: Storages node storageLabel boundaryLabel storageEdgeLabel,
         sequence :: Sequence node structEdge sectionLabel nodeLabel structLabel
      }
   deriving (Eq)

type
   RangeGraph node =
      Graph
         node Graph.EitherEdge ()
         (Node.Type (Maybe Topo.StoreDir))
         () () () ()

{-
Alle Storages sollen in die initiale Sektion,
auch wenn sie nie aktiv sind!
So kann man beim Initialisieren auch Werte zuweisen.
-}
sequenceGraph ::
   (Ord node) =>
   Sequ.List (FlowTopology node) -> RangeGraph node
sequenceGraph sd =
   let sq = fmap Topo.classifyStorages sd
   in  Graph {
          storages =
             fmap
                (storageMapFromList (Fold.toList $ Sequ.mapWithSection const sq) .
                 Flow.storageEdges . Map.mapMaybe id) $
             Flow.getStorageSequences sq,
          sequence = Sequ.toMap $ fmap (FlowTopo.Section ()) sq
       }

flatten ::
   (Ord node) =>
   RangeGraph node -> Flow.RangeGraph node
flatten (Graph tracks sq) =
   (,) (fmap fst sq) $
   Flow.insEdges (fmap (Map.keys . StorageGraph.edges . fst) tracks) $
   Flow.insNodes (Map.keys tracks) $
   Fold.fold $ Map.mapWithKey Flow.sectionFromClassTopo $
   fmap (FlowTopo.topology . snd) sq

{-
Init and Exit sections must be present.
Range map and graph must be consistent.
-}
structure ::
   (Ord node) =>
   Flow.RangeGraph node -> RangeGraph node
structure (rngs, g) =
   let nodes = groupNodes g
       (structEdges, storeEdges) = groupEdges g
   in  Graph {
          storages = fmap (storageMapFromList (Map.keys nodes)) storeEdges,
          sequence =
             Map.intersectionWith (,) rngs $
             fmap (FlowTopo.Section ()) $
             Map.intersectionWith
                (\ns es -> Graph.fromList ns $ map (flip (,) ()) es)
                nodes structEdges
       }

storageMapFromList ::
   (Ord node) =>
   [Idx.Section] ->
   [XIdx.StorageEdge node] ->
   (StorageGraph Idx.Section node () (), Map Idx.Boundary ())
storageMapFromList secs edges =
   (StorageGraph
      (PartMap.constant () secs)
      (Map.fromListWith (error "duplicate storage edge") $
       map (flip (,) ()) edges),
    Map.fromList $ map (flip (,) () . Idx.Following) $
    Idx.Init : map Idx.NoInit secs)

groupEdges ::
   (Ord part, Ord node) =>
   Topo.FlowGraph part node ->
   (Map part [Graph.EitherEdge node],
    Map node [Idx.StorageEdge part node])
groupEdges =
   mapPair (Map.unionsWith (++), Map.unionsWith (++)) .
   ListHT.unzipEithers .
   map
      (\e ->
         case Topo.edgeType e of
            Topo.StructureEdge (Idx.InPart s se) ->
               Left (Map.singleton s [se])
            Topo.StorageEdge (Idx.ForNode se n) ->
               Right (Map.singleton n [se])) .
   Graph.edges

groupNodes ::
   (Ord (e (Idx.AugNode part node)), Ord part, Ord node, Graph.Edge e) =>
   Graph.Graph (Idx.AugNode part node) e nl el ->
   Map part [(node, nl)]
groupNodes =
   Map.fromListWith (++) .
   mapMaybe
      (\(Idx.PartNode aug node, label) ->
         Idx.switchAugmented Nothing Nothing
            (\part -> Just (part, [(node, label)]))
            aug) .
   Graph.labNodes
