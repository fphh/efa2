module EFA.Flow.StorageGraph where

import qualified EFA.Flow.PartMap as PartMap
import EFA.Flow.PartMap (PartMap)

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Utility.Map as MapU
import EFA.Utility.Map (Caller)

import qualified Data.Traversable as Trav
import qualified Data.Map as Map ; import Data.Map (Map)
import Control.Applicative (Applicative, liftA2)


{- |
We could use the generic Graph structure here,
but this one provides more invariants:

* There is always an Init and an Exit node.
* Edges cannot start at Exit and cannot end in Init.
-}
data
   StorageGraph part node nodeLabel edgeLabel =
      StorageGraph {
         nodes :: PartMap part nodeLabel,
         edges :: Map (Idx.StorageEdge part node) edgeLabel
      } deriving (Eq)

mapNode ::
   (Ord part) =>
   (nodeLabel0 -> nodeLabel1) ->
   StorageGraph part node nodeLabel0 edgeLabel ->
   StorageGraph part node nodeLabel1 edgeLabel
mapNode f (StorageGraph partMap edgeMap) =
   StorageGraph (fmap f partMap) edgeMap

mapEdge ::
   (Ord part) =>
   (edgeLabel0 -> edgeLabel1) ->
   StorageGraph part node nodeLabel edgeLabel0 ->
   StorageGraph part node nodeLabel edgeLabel1
mapEdge f (StorageGraph partMap edgeMap) =
   StorageGraph partMap (fmap f edgeMap)

traverse ::
   (Applicative f, Ord part) =>
   (nodeLabel0 -> f nodeLabel1) ->
   (edgeLabel0 -> f edgeLabel1) ->
   StorageGraph part node nodeLabel0 edgeLabel0 ->
   f (StorageGraph part node nodeLabel1 edgeLabel1)
traverse f g (StorageGraph partMap edgeMap) =
   liftA2 StorageGraph
      (Trav.traverse f partMap)
      (Trav.traverse g edgeMap)


lookupEdge ::
   (Ord part) =>
   Idx.StorageEdge part node ->
   StorageGraph part node nodeLabel edgeLabel ->
   Maybe edgeLabel
lookupEdge se =
   Map.lookup se . edges


checkedZipWith ::
   (Ord part) =>
   Caller ->
   (nodeLabel0 -> nodeLabel1 -> nodeLabel2) ->
   (edgeLabel0 -> edgeLabel1 -> edgeLabel2) ->
   StorageGraph part node nodeLabel0 edgeLabel0 ->
   StorageGraph part node nodeLabel1 edgeLabel1 ->
   StorageGraph part node nodeLabel2 edgeLabel2
checkedZipWith name f g
      (StorageGraph partMap0 edges0)
      (StorageGraph partMap1 edges1) =
   StorageGraph
      (PartMap.checkedZipWith (name++".partMap") f partMap0 partMap1)
      (MapU.checkedZipWith (name++".edges") g edges0 edges1)
