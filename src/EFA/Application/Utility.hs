{-# LANGUAGE TypeFamilies #-}
module EFA.Application.Utility where

import qualified EFA.Flow.Sequence.Quantity as SeqFlowQuant
import qualified EFA.Flow.Sequence.Index as SeqIdx
import qualified EFA.Flow.Sequence as SeqFlow

import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology as FlowTopoPlain

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import qualified EFA.Signal.Sequence as Sequ

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
      (MapU.fromSet (const ()) $ foldMap (\(x,y) -> Set.fromList [x,y]) es)
      (Map.fromList $ map (\(a, b) -> (Graph.DirEdge a b, ())) es)


-- @HT neue Utility Funktionen für Topologie-Definition, bitte prüfen
type EdgeLabel = String
type PPosLabel = String

type LabeledEdgeList node = [(node, node, EdgeLabel, PPosLabel, PPosLabel)]
type PPosLabelMap node = Map (SeqIdx.PPos node) String

-- | Generate Topology from labeled edge List
makeTopology ::
   (Node.C node) =>
   LabeledEdgeList node -> Topo.Topology node
makeTopology =
   topologyFromEdges . map (\(n1,n2,_,_,_) -> (n1,n2))


-- | Edge Label map used for displaying topology with labeled edges
makeEdgeNameMap ::
   (Node.C node) => LabeledEdgeList node -> Map (node, node) String
makeEdgeNameMap edgeList = Map.fromList $ map f edgeList
  where f (x, y, lab, _, _) = ((x, y), lab)

-- | Generate Label Map for Power Positions
makePPosLabelMap ::
   (Node.C node) => LabeledEdgeList node -> PPosLabelMap node
makePPosLabelMap edgeList = Map.fromList $ concatMap f edgeList
  where f (n1,n2,_,l1,l2) = [(SeqIdx.ppos n1 n2, l1),
                             (SeqIdx.ppos n2 n1, l2)]


{- |
Construct solvable topology from topology with default directions.
-}
quantityTopology ::
   (Node.C node, SeqFlowQuant.Unknown v) =>
   Topo.Topology node ->
   FlowTopo.Section node v
quantityTopology topo =
   FlowTopo.sectionFromPlain $
   FlowTopoPlain.Section () $
   Topo.classifyStorages $
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
   (Node.C node, SeqFlowQuant.Unknown a, SeqFlowQuant.Unknown v) =>
   Topo.Topology node ->
   [[Graph.EitherEdge node]] ->
   SeqFlowQuant.Graph node a v
seqFlowGraphFromStates topo =
   SeqFlowQuant.graphFromPlain .
   SeqFlow.sequenceGraph .
   fmap (identifyFlowState topo) .
   Sequ.fromList


checkDetermined :: String -> Result a -> a
checkDetermined name rx =
   case rx of
      Undetermined -> error $ "undetermined " ++ name
      Determined x -> x


