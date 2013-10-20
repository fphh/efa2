module EFA.Flow.Sequence where

import qualified EFA.Flow.Topology as FlowTopo
import qualified EFA.Flow.Storage as Storage

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Signal.Sequence as Sequ

import Data.Map (Map)

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
