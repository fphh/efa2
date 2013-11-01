{-# LANGUAGE TypeFamilies #-}
module EFA.Application.Utility where

import qualified EFA.Flow.Sequence.Quantity as SeqFlow

import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import qualified EFA.Signal.Sequence as Sequ

import EFA.Equation.Unknown (Unknown)
import EFA.Equation.Result (Result(Determined, Undetermined))


import qualified EFA.Utility.Map as MapU

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Foldable (foldMap)


topologyFromEdges ::
   Node.C node => [(node, node)] -> Topo.Topology node
topologyFromEdges es =
   Graph.fromMap
      (MapU.fromSet (const ()) $
       foldMap (\(x,y) -> Set.fromList [x,y]) es)
      (Map.fromList $
       map (\(x, y) -> (Graph.DirEdge x y, ())) es)


-- @HT neue Utility Funktionen für Topologie-Definition, bitte prüfen
type EdgeLabel = String
type PPosLabel = String

type LabeledEdgeList node = [(node, node, EdgeLabel, PPosLabel, PPosLabel)]
type PPosLabelMap node = Map (TopoIdx.PPos node) String


topologyFromLabeledEdges ::
   (Node.C node) =>
   LabeledEdgeList node -> Topo.LabeledTopology node
topologyFromLabeledEdges es =
   Graph.fromMap
      (MapU.fromSet (const "") $
       foldMap (\(x,y,_,_,_) -> Set.fromList [x,y]) es)
      (Map.fromList $
       map (\(x, y, lab, _, _) -> (Graph.DirEdge x y, lab)) es)


{- |
Construct solvable topology from topology with default directions.
-}
quantityTopology ::
   (Node.C node, Unknown v) =>
   Topo.Topology node ->
   FlowTopo.Section node v
quantityTopology topo =
   FlowTopo.sectionFromPlain $
   let flowTopo = Graph.mapEdgesMaybe (Just . Graph.EDirEdge) topo
   in  if StateAnalysis.admissibleTopology flowTopo
         then flowTopo
         else error "quantityTopology: topology has forbidden default edges"


dirEdge :: node -> node -> Graph.EitherEdge node
dirEdge x y = Graph.EDirEdge $ Graph.DirEdge x y

undirEdge :: (Node.C node) => node -> node -> Graph.EitherEdge node
undirEdge x y = Graph.EUnDirEdge $ Graph.UnDirEdge x y

identifyFlowState ::
   (Node.C node) =>
   Topo.Topology node -> [Graph.EitherEdge node] -> Topo.FlowTopology node
identifyFlowState topo givenEdges =
   case StateAnalysis.identify topo givenEdges of
      [] -> error "identifyFlowState: impossible given edges"
      [flowTopo] -> flowTopo
      _ -> error "identifyFlowState: ambiguous given edges"

seqFlowGraphFromStates ::
   (Node.C node, Unknown a, Unknown v) =>
   Topo.Topology node ->
   [[Graph.EitherEdge node]] ->
   SeqFlow.Graph node a v
seqFlowGraphFromStates topo =
   seqFlowGraphFromFlowTopos . map (identifyFlowState topo)

seqFlowGraphFromFlowTopos ::
   (Node.C node, Unknown a, Unknown v) =>
   [Topo.FlowTopology node] ->
   SeqFlow.Graph node a v
seqFlowGraphFromFlowTopos =
   SeqFlow.sequenceGraph . Sequ.fromList


checkDetermined :: String -> Result a -> a
checkDetermined name rx =
   case rx of
      Undetermined -> error $ "undetermined " ++ name
      Determined x -> x


