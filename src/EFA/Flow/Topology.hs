module EFA.Flow.Topology where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph
import EFA.Graph (Graph)

import EFA.Utility.Map (Caller)

import Data.Maybe (fromMaybe)



data
   Section node edge sectionLabel nodeLabel edgeLabel =
      Section {
         label :: sectionLabel,
         topology :: Graph.Graph node edge nodeLabel edgeLabel
      }
      deriving (Eq)

mapNode ::
   (nodeLabel0 -> nodeLabel1) ->
   Section node edge sectionLabel nodeLabel0 edgeLabel ->
   Section node edge sectionLabel nodeLabel1 edgeLabel
mapNode f (Section lab topo) =
   Section lab (Graph.mapNode f topo)

mapEdge ::
   (edgeLabel0 -> edgeLabel1) ->
   Section node edge sectionLabel nodeLabel edgeLabel0 ->
   Section node edge sectionLabel nodeLabel edgeLabel1
mapEdge f (Section lab topo) =
   Section lab (Graph.mapEdge f topo)


checkedZipWith ::
   (Ord node, Ord (edge node), Graph.Edge edge) =>
   Caller ->
   (sectionLabel0 -> sectionLabel1 -> sectionLabel2) ->
   (nodeLabel0 -> nodeLabel1 -> nodeLabel2) ->
   (edgeLabel0 -> edgeLabel1 -> edgeLabel2) ->
   Section node edge sectionLabel0 nodeLabel0 edgeLabel0 ->
   Section node edge sectionLabel1 nodeLabel1 edgeLabel1 ->
   Section node edge sectionLabel2 nodeLabel2 edgeLabel2
checkedZipWith caller f g h
      (Section lab0 gr0)
      (Section lab1 gr1) =
   Section
      (f lab0 lab1)
      (Graph.checkedZipWith caller g h gr0 gr1)


lookupEdge ::
   Ord n =>
   (el -> a) ->
   Idx.StructureEdge n ->
   Graph n Graph.EitherEdge nl (Maybe el) ->
   Maybe a
lookupEdge f se =
   fmap (maybe (error "lookupEdge: directed edge must have Just label") f) .
   Graph.lookupEdge (Graph.EDirEdge $ Topo.dirEdgeFromStructureEdge se)


liftEdgeFlow ::
   (Graph.DirEdge node -> flow0 -> flow1) ->
   Graph.EitherEdge node -> Maybe flow0 -> Maybe flow1
liftEdgeFlow f =
   switchEdgeFlow (const Nothing) (\edge flow -> Just $ f edge flow)

switchEdgeFlow ::
   (Graph.UnDirEdge node -> a) ->
   (Graph.DirEdge node -> flow -> a) ->
   Graph.EitherEdge node -> Maybe flow -> a
switchEdgeFlow f _ (Graph.EUnDirEdge edge) Nothing = f edge
switchEdgeFlow _ f (Graph.EDirEdge edge) (Just flow) = f edge flow
switchEdgeFlow _ _ _ _ =
   error $
      "switchEdgeFlow: undirEdge's flow must be Nothing," ++
      " dirEdge's flow must be Just"


dirFromGraph ::
   (Ord n) =>
   Graph n Graph.EitherEdge nl el -> Graph n Graph.DirEdge nl el
dirFromGraph =
   Graph.mapEdgesMaybe $ \ee ->
      case ee of
         Graph.EDirEdge de -> Just de
         Graph.EUnDirEdge _ -> Nothing

dirFromFlowGraph ::
   (Ord n) =>
   Graph n Graph.EitherEdge nl (Maybe el) -> Graph n Graph.DirEdge nl el
dirFromFlowGraph =
   Graph.mapEdge
      (fromMaybe (error "dirFromFlowGraph: directed edge must have Just label"))
   .
   dirFromGraph

dirSectionFromFlowGraph ::
   (Ord n) =>
   Section n Graph.EitherEdge sl nl (Maybe el) ->
   Section n Graph.DirEdge sl nl el
dirSectionFromFlowGraph (Section lab gr) =
   Section lab $ dirFromFlowGraph gr
