{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module EFA.Flow.Topology.Quantity (
   Section, DirSection, FlowTopo.label, FlowTopo.topology,
   Topology, Sums(..), Flow(..),

   mapSection,
   mapTopology,

   checkedZipWithSection,
   checkedZipWithTopology,

   traverseSection,
   traverseTopology,

   mapSectionWithVar,
   mapTopologyWithVar,
   ) where

import qualified EFA.Flow.Topology as FlowTopo
import qualified EFA.Flow.Quantity as Quant
import EFA.Flow.Topology (label, topology)
import EFA.Flow.Quantity
          (Topology, Sums(..), Flow(..),
           mapSums, zipWithSums, traverseSums)

import qualified EFA.Equation.Variable as Var

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import EFA.Utility.Map (Caller)

import Control.Applicative (Applicative, pure, liftA2, (<*>))

import Data.Traversable (Traversable, traverse)

import Prelude hiding (sin)


type
   Section node v =
      FlowTopo.Section node Graph.EitherEdge v (Sums v) (Maybe (Flow v))

type
   DirSection node v =
      FlowTopo.Section node Graph.DirEdge v (Sums v) (Flow v)


mapSection ::
   (v0 -> v1) ->
   Section node v0 -> Section node v1
mapSection f gr =
   FlowTopo.Section {
      label = f $ label gr,
      topology = mapTopology f $ topology gr
   }

mapTopology ::
   (v0 -> v1) ->
   Topology node v0 -> Topology node v1
mapTopology f gr =
   Graph.mapNode (mapSums f) $
   Graph.mapEdge (fmap $ fmap f) gr


checkedZipWithSection ::
   (Ord node) =>
   Caller ->
   (v0 -> v1 -> v2) ->
   Section node v0 ->
   Section node v1 ->
   Section node v2
checkedZipWithSection caller f gr0 gr1 =
   FlowTopo.Section {
      label = f (label gr0) (label gr1),
      topology = checkedZipWithTopology caller f (topology gr0) (topology gr1)
   }

checkedZipWithTopology ::
   (Ord node) =>
   Caller ->
   (v0 -> v1 -> v2) ->
   Topology node v0 ->
   Topology node v1 ->
   Topology node v2
checkedZipWithTopology caller f gr0 gr1 =
   Graph.checkedZipWith
      (caller++".checkedZipWithTopology.section")
      (zipWithSums f)
      (liftA2 $ liftA2 f)
      gr0 gr1


traverseSection ::
   (Applicative f, Ord node) =>
   (v0 -> f v1) ->
   Section node v0 -> f (Section node v1)
traverseSection f (FlowTopo.Section lab topo) =
   liftA2 FlowTopo.Section (f lab) (traverseTopology f topo)

traverseTopology ::
   (Applicative f, Ord node) =>
   (v0 -> f v1) ->
   Topology node v0 -> f (Topology node v1)
traverseTopology f =
   Graph.traverse (traverseSums f) (traverse $ traverse f)


mapSectionWithVar ::
   (Ord node) =>
   (Var.Signal node -> v0 -> v1) ->
   Section node v0 ->
   Section node v1
mapSectionWithVar f gr =
   FlowTopo.Section {
      label = f (Var.DTime Idx.DTime) $ label gr,
      topology = mapTopologyWithVar f $ topology gr
   }

mapTopologyWithVar ::
   (Ord node) =>
   (Var.Signal node -> v0 -> v1) ->
   Topology node v0 ->
   Topology node v1
mapTopologyWithVar f topo =
   Graph.mapNodeWithKey
      (\n (Sums {sumIn = sin, sumOut = sout}) ->
         Sums {
            sumIn = flip fmap sin $ f (Var.signalIndex $ Idx.Sum Idx.In n),
            sumOut = flip fmap sout $ f (Var.signalIndex $ Idx.Sum Idx.Out n)
         }) $
   Graph.mapEdgeWithKey (Quant.liftEdgeFlow $ mapFlowWithVar f) topo

mapFlowWithVar ::
   (Var.Signal node -> v0 -> v1) ->
   Graph.DirEdge node -> Flow v0 -> Flow v1
mapFlowWithVar f e =
   liftA2 f
      (Quant.flowVars <*> pure (Topo.structureEdgeFromDirEdge e))
