module EFA.Flow.State where

import qualified EFA.Flow.Topology as FlowTopo
import EFA.Flow.StorageGraph (StorageGraph)

import qualified EFA.Graph.Topology.Index as Idx

import Data.Map (Map)


type
   Storages node storageLabel storageEdgeLabel =
      Map node
         (StorageGraph Idx.State node storageLabel storageEdgeLabel)

type
   States node structEdge stateLabel nodeLabel structLabel =
      Map Idx.State
         (FlowTopo.Section node structEdge stateLabel nodeLabel structLabel)

data
   Graph node structEdge
         stateLabel nodeLabel storageLabel
         structLabel storageEdgeLabel =
      Graph {
         storages :: Storages node storageLabel storageEdgeLabel,
         states :: States node structEdge stateLabel nodeLabel structLabel
      }
