{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module EFA.Flow.Topology.Quantity (
   Section, DirSection, FlowTopo.label, FlowTopo.topology,
   Topology, DirTopology, Sums(..), Flow(..),

   mapSection,
   mapTopology,

   checkedZipWithSection,
   checkedZipWithTopology,

   traverseSection,
   traverseTopology,

   mapSectionWithVar,
   mapTopologyWithVar,
   mapFlowWithVar,

   FlowTopo.liftEdgeFlow,
   dirFromSums,

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
import EFA.Flow.Topology (label, topology)

import EFA.Equation.Unknown (Unknown(unknown))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import EFA.Utility.Map (Caller)

import Control.Monad (mplus)
import Control.Applicative (Applicative, pure, liftA2, (<*>))

import qualified Data.Foldable as Fold
import Data.Traversable (Traversable, traverse, foldMapDefault)
import Data.Foldable (Foldable)
import Data.Maybe.HT (toMaybe)

import Prelude hiding (lookup, sin)



type
   Section node v =
      FlowTopo.Section node Graph.EitherEdge v (Sums v) (Maybe (Flow v))

type
   DirSection node v =
      FlowTopo.Section node Graph.DirEdge v (Sums v) (Flow v)

type
   Topology node v =
      Graph.Graph node Graph.EitherEdge (Sums v) (Maybe (Flow v))

type
   DirTopology node a v =
      Graph.Graph node Graph.DirEdge (Sums v) (Flow v)


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
            (FlowTopo.lookupEdge fieldOut se topo)
            (FlowTopo.lookupEdge fieldIn (Idx.flip se) topo)


lookupEta :: (Ord node) => Idx.Eta node -> Section node v -> Maybe v
lookupEta (Idx.Eta se) = FlowTopo.lookupEdge flowEta se . FlowTopo.topology

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
   Graph.mapEdgeWithKey (FlowTopo.liftEdgeFlow $ mapFlowWithVar f) topo

mapFlowWithVar ::
   (Var.Signal node -> v0 -> v1) ->
   Graph.DirEdge node -> Flow v0 -> Flow v1
mapFlowWithVar f e =
   liftA2 f
      (flowVars <*> pure (Topo.structureEdgeFromDirEdge e))


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



data Flow v =
   Flow {
      flowPowerOut, flowEnergyOut, flowXOut,
      flowEta,
      flowXIn, flowEnergyIn, flowPowerIn :: v
   }
   deriving (Eq)

data Sums v =
   Sums { sumIn, sumOut :: Maybe v }
   deriving (Eq)


instance Functor Flow where
   fmap f (Flow pout eout xout eta xin ein pin) =
      Flow (f pout) (f eout) (f xout) (f eta) (f xin) (f ein) (f pin)

instance Foldable Flow where
   foldMap = foldMapDefault

instance Traversable Flow where
   traverse f (Flow pout eout xout eta xin ein pin) =
      pure Flow <*> f pout <*> f eout <*> f xout <*> f eta <*> f xin <*> f ein <*> f pin

instance Applicative Flow where
   pure a = Flow a a a a a a a
   Flow fpout feout fxout feta fxin fein fpin
         <*> Flow pout eout xout eta xin ein pin =
      Flow
         (fpout pout) (feout eout) (fxout xout)
         (feta eta) (fxin xin) (fein ein) (fpin pin)


instance Functor Sums where
   fmap f (Sums i o) = Sums (fmap f i) (fmap f o)

instance Foldable Sums where
   foldMap = foldMapDefault

instance Traversable Sums where
   traverse f (Sums i o) = liftA2 Sums (traverse f i) (traverse f o)

instance Applicative Sums where
   pure a = Sums (Just a) (Just a)
   (Sums fi fo) <*> (Sums i o) = Sums (fi <*> i) (fo <*> o)


mapSums ::
   (v0 -> v1) ->
   Sums v0 -> Sums v1
mapSums f s =
   Sums {
      sumIn  = fmap f $ sumIn  s,
      sumOut = fmap f $ sumOut s
   }


zipWithSums ::
   (v0 -> v1 -> v2) ->
   Sums v0 -> Sums v1 -> Sums v2
zipWithSums f s0 s1 =
   Sums {
      sumIn  = liftA2 f (sumIn  s0) (sumIn  s1),
      sumOut = liftA2 f (sumOut s0) (sumOut s1)
   }


traverseSums ::
   (Applicative f) =>
   (v0 -> f v1) ->
   Sums v0 -> f (Sums v1)
traverseSums f (Sums i o) =
   liftA2 Sums
      (traverse f i)
      (traverse f o)


flowVars :: Flow (Idx.StructureEdge node -> Var.Signal node)
flowVars =
   Flow {
      flowPowerOut = Var.index . Idx.Power,
      flowPowerIn = Var.index . Idx.Power . Idx.flip,
      flowEnergyOut = Var.index . Idx.Energy,
      flowEnergyIn = Var.index . Idx.Energy . Idx.flip,
      flowXOut = Var.index . Idx.X,
      flowXIn = Var.index . Idx.X . Idx.flip,
      flowEta = Var.index . Idx.Eta
   }


dirFromSums :: Sums v -> Maybe Topo.StoreDir
dirFromSums sums =
   case (sumIn sums, sumOut sums) of
      (Nothing, Nothing) -> Nothing
      (Just _, Nothing) -> Just Topo.In
      (Nothing, Just _) -> Just Topo.Out
      (Just _, Just _) -> error "storage cannot be both In and Out"