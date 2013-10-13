module EFA.Flow.Topology where

import qualified EFA.Flow.Quantity as Quant

import qualified EFA.Graph as Graph

import EFA.Utility.Map (Caller)


data
   Section node edge sectionLabel nodeLabel edgeLabel =
      Section {
         label :: sectionLabel,
         topology :: Graph.Graph node edge nodeLabel edgeLabel
      }
      deriving (Eq)

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

dirFromFlowGraph ::
   (Ord n) =>
   Section n Graph.EitherEdge sl nl (Maybe el) ->
   Section n Graph.DirEdge sl nl el
dirFromFlowGraph (Section lab gr) =
   Section lab $ Quant.dirFromFlowGraph gr
