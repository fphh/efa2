module EFA.Flow.State where

import qualified EFA.Flow.Topology as FlowTopo
import qualified EFA.Flow.Storage as Storage

import qualified EFA.Flow.SequenceState.Index as Idx

import Data.Map (Map)


type
   Storages node storageLabel carryLabel =
      Map node
         (Storage.Graph Idx.State storageLabel carryLabel)

type
   States node edge stateLabel nodeLabel flowLabel =
      Map Idx.State
         (FlowTopo.Section node edge stateLabel nodeLabel flowLabel)

data
   Graph node edge
         stateLabel nodeLabel storageLabel
         flowLabel carryLabel =
      Graph {
         storages :: Storages node storageLabel carryLabel,
         states :: States node edge stateLabel nodeLabel flowLabel
      }


instance Show (Graph a b c d e f g) where
         show _ = "<State.Graph>"
