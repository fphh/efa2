module EFA.Flow.Sequence where

import qualified EFA.Flow.Sequence.Index as XIdx

import qualified EFA.Graph.Flow as Flow

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph
import EFA.Graph.Topology (FlowTopology)

import qualified EFA.Signal.SequenceData as SD

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Foldable as Fold
import qualified Data.List.HT as ListHT

import Data.Tuple.HT (mapPair, thd3)
import Data.Maybe (mapMaybe)

import Prelude hiding (sequence)


type
   Storages node initLabel exitLabel boundaryLabel storageLabel =
      Map node
         ((initLabel, exitLabel),
          Map Idx.Boundary boundaryLabel,
          Map (XIdx.StorageEdge node) storageLabel)

type
   Sequence node structEdge sectionLabel nodeLabel structLabel =
      SD.Map
         (sectionLabel, Graph.Graph node structEdge nodeLabel structLabel)

data
   Graph node structEdge
         sectionLabel nodeLabel initLabel exitLabel boundaryLabel
         structLabel storageLabel =
      Graph {
         storages :: Storages node initLabel exitLabel boundaryLabel storageLabel,
         sequence :: Sequence node structEdge sectionLabel nodeLabel structLabel
      }
   deriving (Eq)

type
   RangeGraph node =
      Graph
         node Graph.EitherEdge ()
         (Node.Type (Maybe Topo.StoreDir))
         InitIn ExitOut () () ()

data InitIn  = InitIn
data ExitOut = ExitOut

{-
Alle Storages sollen in die initiale Sektion,
auch wenn sie nie aktiv sind!
So kann man beim Initialisieren auch Werte zuweisen.
-}
sequenceGraph ::
   (Ord node) =>
   SD.SequData (FlowTopology node) -> RangeGraph node
sequenceGraph sd =
   let sq = fmap Topo.classifyStorages sd
   in  Graph {
          storages =
             fmap
                (storageMapFromList (Fold.toList $ SD.mapWithSection const sq) .
                 Flow.storageEdges . Map.mapMaybe id) $
             Flow.getStorageSequences sq,
          sequence = SD.toMap $ fmap ((,) ()) sq
       }

flatten ::
   (Ord node) =>
   RangeGraph node -> Flow.RangeGraph node
flatten (Graph tracks sq) =
   (,) (fmap fst sq) $
   Flow.insEdges (fmap (Map.keys . thd3) tracks) $
   Flow.insNodes (Map.keys tracks) $
   Fold.fold $ Map.mapWithKey Flow.sectionFromClassTopo $ fmap (snd . snd) sq

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
             fmap ((,) ()) $
             Map.intersectionWith
                (\ns es -> Graph.fromList ns $ map (flip (,) ()) es)
                nodes structEdges
       }

storageMapFromList ::
   (Ord e) =>
   [Idx.Section] ->
   [e] ->
   ((InitIn, ExitOut), Map Idx.Boundary (), Map e ())
storageMapFromList secs =
   (,,)
      (InitIn, ExitOut)
      (Map.fromList $ map (flip (,) () . Idx.Following) $
       Idx.Init : map Idx.NoInit secs).
   Map.fromListWith (error "duplicate storage edge") .
   map (flip (,) ())

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
