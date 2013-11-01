module EFA.Flow.Storage where

import qualified EFA.Flow.Part.Map as PartMap
import EFA.Flow.Part.Map (PartMap)

import qualified EFA.Flow.Storage.Index as StorageIdx
import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Utility.Map as MapU
import EFA.Utility.Map (Caller)

import qualified Data.Traversable as Trav
import qualified Data.Map as Map ; import Data.Map (Map)

import Control.Applicative (Applicative, liftA2)
import Data.Foldable (Foldable, fold)
import Data.Monoid (Monoid)


{- |
We could use the generic Graph structure here,
but this one provides more invariants:

* There is always an Init and an Exit node.
* Edges cannot start at Exit and cannot end in Init.
-}
data
   Graph part nodeLabel edgeLabel =
      Graph {
         nodes :: PartMap part nodeLabel,
         edges :: Map (StorageIdx.Edge part) edgeLabel
      } deriving (Eq)

mapNode ::
   (Ord part) =>
   (nodeLabel0 -> nodeLabel1) ->
   Graph part nodeLabel0 edgeLabel ->
   Graph part nodeLabel1 edgeLabel
mapNode f (Graph partMap edgeMap) =
   Graph (fmap f partMap) edgeMap

mapEdge ::
   (Ord part) =>
   (edgeLabel0 -> edgeLabel1) ->
   Graph part nodeLabel edgeLabel0 ->
   Graph part nodeLabel edgeLabel1
mapEdge f (Graph partMap edgeMap) =
   Graph partMap (fmap f edgeMap)

traverse ::
   (Applicative f, Ord part) =>
   (nodeLabel0 -> f nodeLabel1) ->
   (edgeLabel0 -> f edgeLabel1) ->
   Graph part nodeLabel0 edgeLabel0 ->
   f (Graph part nodeLabel1 edgeLabel1)
traverse f g (Graph partMap edgeMap) =
   liftA2 Graph
      (Trav.traverse f partMap)
      (Trav.traverse g edgeMap)


lookupEdge ::
   (Ord part) =>
   StorageIdx.Edge part ->
   Graph part nodeLabel edgeLabel ->
   Maybe edgeLabel
lookupEdge se =
   Map.lookup se . edges


checkedZipWith ::
   (Ord part) =>
   Caller ->
   (nodeLabel0 -> nodeLabel1 -> nodeLabel2) ->
   (edgeLabel0 -> edgeLabel1 -> edgeLabel2) ->
   Graph part nodeLabel0 edgeLabel0 ->
   Graph part nodeLabel1 edgeLabel1 ->
   Graph part nodeLabel2 edgeLabel2
checkedZipWith name f g
      (Graph partMap0 edges0)
      (Graph partMap1 edges1) =
   Graph
      (PartMap.checkedZipWith (name++".partMap") f partMap0 partMap1)
      (MapU.checkedZipWith (name++".edges") g edges0 edges1)


foldInStorages ::
   (Ord part, Monoid m) =>
   (Idx.Init part -> [a] -> m) -> Map (StorageIdx.Edge part) a -> m
foldInStorages f =
   fold .
   Map.mapWithKey (\sec outs -> f sec (Map.elems outs)) .
   MapU.curry "foldInStorages"
      (\(StorageIdx.Edge from to) -> (from, to))

foldOutStorages ::
   (Ord part, Monoid m) =>
   (Idx.Exit part -> [a] -> m) -> Map (StorageIdx.Edge part) a -> m
foldOutStorages f =
   fold .
   Map.mapWithKey (\sec ins -> f sec (Map.elems ins)) .
   MapU.curry "foldOutStorages"
      (\(StorageIdx.Edge from to) -> (to, from))
