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

   sectionFromPlain,
   unknownTopologyNodes,

   lookupPower,
   lookupEnergy,
   lookupX,
   lookupEta,
   lookupSum,
   lookupDTime,
   lookupSums,

   Lookup, lookup,
   ) where

import qualified EFA.Flow.Topology.Variable as Var
import qualified EFA.Flow.Topology as FlowTopo
import qualified EFA.Flow.Quantity as Quant
import EFA.Flow.Topology (label, topology)
import EFA.Flow.Quantity
          (Topology, Sums(..), Flow(..),
           mapSums, zipWithSums, traverseSums)

import EFA.Equation.Unknown (Unknown(unknown))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import EFA.Utility.Map (Caller)

import Control.Monad (mplus)
import Control.Applicative (Applicative, pure, liftA2, (<*>))

import Data.Traversable (Traversable, traverse)
import Data.Maybe.HT (toMaybe)

import Prelude hiding (lookup, sin)


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


lookupPower ::
   (Ord node) => Idx.Power node -> Section node v -> Maybe v
lookupPower =
   lookupStruct flowPowerOut flowPowerIn (\(Idx.Power se) -> se)

lookupEnergy ::
   (Ord node) => Idx.Energy node -> Section node v -> Maybe v
lookupEnergy =
   lookupStruct flowEnergyOut flowEnergyIn (\(Idx.Energy se) -> se)

lookupX ::
   (Ord node) => Idx.X node -> Section node v -> Maybe v
lookupX =
   lookupStruct flowXOut flowXIn (\(Idx.X se) -> se)

lookupStruct ::
   Ord node =>
   (Flow v -> v) ->
   (Flow v -> v) ->
   (idx -> Idx.StructureEdge node) ->
   idx -> Section node v -> Maybe v
lookupStruct fieldOut fieldIn unpackIdx idx (FlowTopo.Section _lab topo) =
   case unpackIdx idx of
      se ->
         mplus
            (Quant.lookupEdge fieldOut se topo)
            (Quant.lookupEdge fieldIn (Idx.flip se) topo)


lookupEta :: (Ord node) => Idx.Eta node -> Section node v -> Maybe v
lookupEta (Idx.Eta se) = Quant.lookupEdge flowEta se . FlowTopo.topology

lookupSum :: (Ord node) => Idx.Sum node -> Section node v -> Maybe v
lookupSum (Idx.Sum dir node) s = do
   sums <- Graph.lookupNode node $ FlowTopo.topology s
   case dir of
      Idx.In  -> sumIn sums
      Idx.Out -> sumOut sums

lookupSums :: (Ord node) => node -> Section node v -> Maybe (Sums v)
lookupSums node = Graph.lookupNode node . FlowTopo.topology

lookupDTime :: Idx.DTime node -> Section node v -> Maybe v
lookupDTime Idx.DTime = Just . FlowTopo.label


class (Var.FormatIndex idx) => Lookup idx where
   lookup :: (Ord node) => idx node -> Section node v -> Maybe v

instance Lookup Idx.Energy where
   lookup = lookupEnergy

instance Lookup Idx.Power where
   lookup = lookupPower

instance Lookup Idx.Eta where
   lookup = lookupEta

instance Lookup Idx.DTime where
   lookup = lookupDTime

instance Lookup Idx.X where
   lookup = lookupX

instance Lookup Idx.Sum where
   lookup = lookupSum


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
            sumIn = flip fmap sin $ f (Var.Sum $ Idx.Sum Idx.In n),
            sumOut = flip fmap sout $ f (Var.Sum $ Idx.Sum Idx.Out n)
         }) $
   Graph.mapEdgeWithKey (Quant.liftEdgeFlow $ mapFlowWithVar f) topo

mapFlowWithVar ::
   (Var.Signal node -> v0 -> v1) ->
   Graph.DirEdge node -> Flow v0 -> Flow v1
mapFlowWithVar f e =
   liftA2 f
      (Quant.flowVars <*> pure (Topo.structureEdgeFromDirEdge e))


sectionFromPlain ::
   (Ord node, Unknown v) =>
   FlowTopo.Section node Graph.EitherEdge () (Node.Type (Maybe Topo.StoreDir)) () ->
   Section node v
sectionFromPlain (FlowTopo.Section () gr) =
   FlowTopo.Section unknown $
   unknownTopologyNodes $
   Graph.mapEdgeWithKey
      (\ee _ ->
         case ee of
            Graph.EUnDirEdge _ -> Nothing
            Graph.EDirEdge _ -> Just $ pure unknown) gr

unknownTopologyNodes ::
   (Ord node, Unknown v) =>
   Graph.Graph node Graph.EitherEdge nl el ->
   Graph.Graph node Graph.EitherEdge (Sums v) el
unknownTopologyNodes =
   Graph.mapNodeWithInOut
      (\(pre, _, suc) ->
         let maybeDir es =
                toMaybe (any (Topo.isActive . fst) es) unknown
         in  Sums {sumIn = maybeDir pre, sumOut = maybeDir suc})
