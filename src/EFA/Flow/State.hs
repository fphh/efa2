module EFA.Flow.State where

import qualified EFA.Flow.Topology as FlowTopo
import qualified EFA.Flow.Storage as Storage

import qualified EFA.Graph.Topology.Index as Idx

import Data.Map (Map)


type
   Storages node storageLabel carryEdgeLabel =
      Map node
         (Storage.Graph Idx.State node storageLabel carryEdgeLabel)

type
   States node structEdge stateLabel nodeLabel structLabel =
      Map Idx.State
         (FlowTopo.Section node structEdge stateLabel nodeLabel structLabel)

data
   Graph node structEdge
         stateLabel nodeLabel storageLabel
         structLabel carryEdgeLabel =
      Graph {
         storages :: Storages node storageLabel carryEdgeLabel,
         states :: States node structEdge stateLabel nodeLabel structLabel
      }
