module EFA.Flow.State where

import qualified EFA.Flow.State.Index as XIdx

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph as Graph

import Data.Map (Map)


type
   Storages node initLabel exitLabel storageLabel =
      Map node
         ((initLabel, exitLabel),
          Map (XIdx.StorageEdge node) storageLabel)

type
   States node structEdge stateLabel nodeLabel structLabel =
      Map Idx.State
         (stateLabel, Graph.Graph node structEdge nodeLabel structLabel)

data
   Graph node structEdge
         stateLabel nodeLabel initLabel exitLabel
         structLabel storageLabel =
      Graph {
         storages :: Storages node initLabel exitLabel storageLabel,
         states :: States node structEdge stateLabel nodeLabel structLabel
      }
