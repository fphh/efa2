module EFA.Flow.Sequence where

import qualified EFA.Flow.Topology as FlowTopo
import qualified EFA.Flow.Storage as Storage

import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Signal.Sequence as Sequ

import Data.Map (Map)

import Prelude hiding (sequence)


type
   Storages node storageLabel boundaryLabel carryEdgeLabel =
      Map node
         (Storage.Graph Idx.Section storageLabel carryEdgeLabel,
          Map Idx.Boundary boundaryLabel)

type
   Sequence node structEdge sectionLabel nodeLabel structLabel =
      Sequ.Map
         (FlowTopo.Section node structEdge sectionLabel nodeLabel structLabel)

data
   Graph node structEdge
         sectionLabel nodeLabel storageLabel boundaryLabel
         structLabel carryEdgeLabel =
      Graph {
         storages :: Storages node storageLabel boundaryLabel carryEdgeLabel,
         sequence :: Sequence node structEdge sectionLabel nodeLabel structLabel
      }
   deriving (Eq)
