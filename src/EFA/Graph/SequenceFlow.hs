module EFA.Graph.SequenceFlow where

import qualified EFA.Application.Index as XIdx

import qualified EFA.Graph.Flow as Flow

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr
import EFA.Graph.Topology (FlowTopology)

import qualified EFA.Signal.SequenceData as SD

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Foldable as Fold
import qualified Data.List.HT as ListHT

import Data.Tuple.HT (mapPair)
import Data.Maybe (mapMaybe)

import Prelude hiding (sequence)


data
   Graph node structEdge
         nodeLabel initLabel exitLabel structLabel storageLabel =
      Graph {
         storages ::
            Map node
               ((initLabel, exitLabel),
                Map (XIdx.StorageEdge node) storageLabel),
         sequence ::
            SD.SequData
               (Gr.Graph node structEdge nodeLabel structLabel)
      }

type
   RangeGraph node =
      Graph
         node Gr.EitherEdge
         (Topo.NodeType (Maybe Topo.StoreDir))
         InitIn ExitOut () ()

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
                (storageMapFromList . Flow.storageEdges . Map.mapMaybe id) $
             Flow.getStorageSequences sq,
          sequence = sq
       }

flatten ::
   (Ord node) =>
   RangeGraph node -> Flow.RangeGraph node
flatten (Graph tracks sq) =
   (,)
      (Fold.fold $
       SD.mapWithSectionRange (\s rng _ -> Map.singleton s rng) sq) $
   Flow.insEdges (fmap (Map.keys . snd) tracks) $
   Flow.insNodes (Map.keys tracks) $
   Fold.fold $ SD.mapWithSection Flow.sectionFromClassTopo sq

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
          storages = fmap storageMapFromList storeEdges,
          sequence =
             SD.SequData $
             map (\(sec, (rng, topo)) -> SD.Section sec rng topo) $
             Map.toAscList $
             Map.intersectionWith (,) rngs $
             Map.intersectionWith
                (\ns es -> Gr.fromList ns $ map (flip (,) ()) es)
                nodes structEdges
       }

storageMapFromList ::
   (Ord e) =>
   [e] -> ((InitIn, ExitOut), Map e ())
storageMapFromList =
   (,) (InitIn, ExitOut) .
   Map.fromListWith (error "duplicate storage edge") .
   map (flip (,) ())

groupEdges ::
   (Ord part, Ord node) =>
   Topo.FlowGraph part node ->
   (Map part [Gr.EitherEdge node],
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
   Gr.edges

groupNodes ::
   (Ord (e (Idx.AugNode part node)), Ord part, Ord node, Gr.Edge e) =>
   Gr.Graph (Idx.AugNode part node) e nl el ->
   Map part [(node, nl)]
groupNodes =
   Map.fromListWith (++) .
   mapMaybe
      (\(Idx.PartNode aug node, label) ->
         Idx.switchAugmented Nothing Nothing
            (\part -> Just (part, [(node, label)]))
            aug) .
   Gr.labNodes
