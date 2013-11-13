module EFA.Flow.Sequence where

import qualified EFA.Flow.Topology as FlowTopo
import qualified EFA.Flow.Storage as Storage

import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Signal.Sequence as Sequ

import Data.Map (Map)

import Prelude hiding (sequence)


type
   Storages node storageLabel boundaryLabel carryLabel =
      Map node
         (Storage.Graph Idx.Section storageLabel carryLabel,
          Map Idx.Boundary boundaryLabel)

type
   Sequence node edge sectionLabel nodeLabel flowLabel =
      Sequ.Map
         (FlowTopo.Section node edge sectionLabel nodeLabel flowLabel)

data
   Graph node edge
         sectionLabel nodeLabel storageLabel boundaryLabel
         flowLabel carryLabel =
      Graph {
         storages :: Storages node storageLabel boundaryLabel carryLabel,
         sequence :: Sequence node edge sectionLabel nodeLabel flowLabel
      }
   deriving (Eq)
