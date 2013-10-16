module EFA.Flow.State where

import qualified EFA.Flow.State.Index as XIdx

import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Flow.StorageGraph.Quantity as StorageGraph
import qualified EFA.Flow.PartMap as PartMap
import EFA.Flow.StorageGraph (StorageGraph(StorageGraph))

import qualified EFA.Graph.StateFlow as StateFlow

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import qualified Data.Map as Map ; import Data.Map (Map)


type
   Storages node storageLabel storageEdgeLabel =
      Map node
         (StorageGraph Idx.State node storageLabel storageEdgeLabel)

type
   States node structEdge stateLabel nodeLabel structLabel =
      Map Idx.State
         (FlowTopoPlain.Section node structEdge stateLabel nodeLabel structLabel)

data
   Graph node structEdge
         stateLabel nodeLabel storageLabel
         structLabel storageEdgeLabel =
      Graph {
         storages :: Storages node storageLabel storageEdgeLabel,
         states :: States node structEdge stateLabel nodeLabel structLabel
      }


flowGraphFromStates ::
   (Node.C node) =>
   [Topo.FlowTopology node] ->
   Graph node Graph.EitherEdge () (Node.Type (Maybe Topo.StoreDir)) () () ()
flowGraphFromStates flowStates =
   let numFlowStates =
          zip [Idx.State 0 ..] $ map Topo.classifyStorages flowStates
   in  Graph {
          storages =
             fmap
                (storageMapFromList (map fst numFlowStates) .
                 StorageGraph.allEdgesFromSums .
                 fmap (FlowTopo.sumsFromDir ())) $
             StateFlow.getStorageSequences $
             Map.fromList numFlowStates,
          states =
             fmap (FlowTopoPlain.Section ()) $ Map.fromList numFlowStates
       }

storageMapFromList ::
   (Ord node) =>
   [Idx.State] ->
   [XIdx.StorageEdge node] ->
   StorageGraph Idx.State node () ()
storageMapFromList sts edges =
   StorageGraph
      (PartMap.constant () sts)
      (Map.fromListWith (error "duplicate storage edge") $
       map (flip (,) ()) edges)

storageSequences ::
   (Node.C node) =>
   [(Idx.State, Graph.Graph node Graph.EitherEdge (FlowTopo.Sums v) edgeLabel)] ->
   Map node (Map Idx.State (FlowTopo.Sums v))
storageSequences =
   Map.unionsWith (Map.unionWith (error "duplicate section for node"))
   .
   map
      (\(s, topo) ->
         fmap (Map.singleton s) $
         Map.filterWithKey (const . Topo.isStorage . Node.typ) $
         Graph.nodeLabels topo)
