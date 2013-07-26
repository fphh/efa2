module EFA.Graph.SequenceFlow where

import qualified EFA.Application.Index as XIdx

import qualified EFA.Graph.Flow as Flow

import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr
import EFA.Graph.Topology (FlowTopology)

import qualified EFA.Signal.SequenceData as SD

import qualified Data.Map as Map ; import Data.Map (Map)


data
   SequenceFlowGraph node structEdge
         nodeLabel initLabel exitLabel structLabel storageLabel =
      FlowGraph {
         storages ::
            Map node
               ((initLabel, exitLabel),
                Map (XIdx.StorageEdge node) storageLabel),
         sequ ::
            SD.SequData
               (Gr.Graph node structEdge nodeLabel structLabel)
      }

type
   RangeGraph node =
      SequenceFlowGraph
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
   in  FlowGraph {
          storages =
             fmap
                ((,) (InitIn, ExitOut) .
                 Map.fromListWith (error "duplicate storage edge") .
                 map (flip (,) ()) . Flow.storageEdges . Map.mapMaybe id) $
             Flow.getStorageSequences sq,
          sequ = sq
       }
