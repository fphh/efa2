module EFA.Flow.Sequence where

import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Topology as FlowTopo
import qualified EFA.Flow.Storage as Storage
import qualified EFA.Flow.PartMap as PartMap

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph
import EFA.Graph.Topology (FlowTopology)

import qualified EFA.Signal.Sequence as Sequ

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Foldable as Fold

import Prelude hiding (sequence)


type
   Storages node storageLabel boundaryLabel storageEdgeLabel =
      Map node
         (Storage.Graph Idx.Section node storageLabel storageEdgeLabel,
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
                 storageEdges . Map.mapMaybe id) $
             storageSequences sq,
          sequence = Sequ.toMap $ fmap (FlowTopo.Section ()) sq
       }

storageMapFromList ::
   (Ord node) =>
   [Idx.Section] ->
   [XIdx.StorageEdge node] ->
   (Storage.Graph Idx.Section node () (), Map Idx.Boundary ())
storageMapFromList secs edges =
   (Storage.Graph
      (PartMap.constant () secs)
      (Map.fromListWith (error "duplicate storage edge") $
       map (flip (,) ()) edges),
    Map.fromList $ map (flip (,) () . Idx.Following) $
    Idx.Init : map Idx.NoInit secs)

storageEdges ::
   Map Idx.Section Topo.StoreDir -> [Idx.StorageEdge Idx.Section node]
storageEdges stores = do
   let (ins, outs) = Map.partition (Topo.In ==) stores
   secin <- Idx.Init : map Idx.NoInit (Map.keys ins)
   secout <-
      (++[Idx.Exit]) $ map Idx.NoExit $ Map.keys $
      case secin of
         Idx.Init -> outs
         Idx.NoInit s -> snd $ Map.split s outs
   return $ Idx.StorageEdge secin secout

storageSequences ::
   (Ord node) =>
   Sequ.List (Topo.ClassifiedTopology node) ->
   Map node (Map Idx.Section (Maybe Topo.StoreDir))
storageSequences =
   Map.unionsWith (Map.unionWith (error "duplicate section for node"))
   .
   Fold.toList
   .
   Sequ.mapWithSection
      (\s g ->
         fmap (Map.singleton s) $
         Map.mapMaybe Topo.maybeStorage $ Graph.nodeLabels g)
