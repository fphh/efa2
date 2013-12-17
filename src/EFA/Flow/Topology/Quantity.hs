{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module EFA.Flow.Topology.Quantity (
   Section, DirSection, FlowTopo.label, FlowTopo.topology,
   Topology, DirTopology, Sums(..), Flow(..), Label(..),

   mapSection, mapDirSection,
   mapTopology, mapDirTopology,

   checkedZipWithSection,
   checkedZipWithTopology,

   traverseSection, traverseDirSection,
   traverseTopology, traverseDirTopology,

   mapSectionWithVar,
   mapTopologyWithVar,
   mapSumsWithVar,
   mapFlowWithVar,

   FlowTopo.liftEdgeFlow,
   dirFromSums,
   sumsFromDir,

   sectionFromPlain,
   unknownTopologyNodes,

   toAssignMap,

   lookupPower,
   lookupEnergy,
   lookupX,
   lookupEta,
   lookupSum,
   lookupDTime,
   lookupOne,
   lookupSums,
   lookupAutoDirSection,

   Lookup, lookup,

   fold, foldDir,
   foldMap, foldMapDir,
   ) where

import qualified EFA.Flow.Topology.Variable as Var
import qualified EFA.Flow.Topology.AssignMap as AssignMap
import qualified EFA.Flow.Topology.Index as Idx
import qualified EFA.Flow.Topology as FlowTopo
import EFA.Flow.Topology.AssignMap (AssignMap)
import EFA.Flow.Topology (label, topology)

import EFA.Equation.Unknown (Unknown(unknown))

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import EFA.Utility.Map (Caller)

import qualified Control.Monad.Trans.Writer as MW
import Control.Monad (mplus)
import Control.Applicative (Applicative, pure, liftA2, (<*>))

import qualified Data.Foldable as Fold
import Data.Traversable (Traversable, traverse, foldMapDefault)
import Data.Foldable (Foldable)
import Data.Maybe.HT (toMaybe)
import Data.Monoid (Monoid)

import Prelude hiding (lookup, sin)



type
   Section node v =
      FlowTopo.Section node Graph.EitherEdge (Label v) (Sums v) (Maybe (Flow v))

type
   DirSection node v =
      FlowTopo.Section node Graph.DirEdge (Label v) (Sums v) (Flow v)

type
   Topology node v =
      Graph.Graph node Graph.EitherEdge (Sums v) (Maybe (Flow v))

type
   DirTopology node v =
      Graph.Graph node Graph.DirEdge (Sums v) (Flow v)


mapSection ::
   (v0 -> v1) ->
   Section node v0 -> Section node v1
mapSection f gr =
   FlowTopo.Section {
      label = fmap f $ label gr,
      topology = mapTopology f $ topology gr
   }

mapDirSection ::
   (v0 -> v1) ->
   DirSection node v0 -> DirSection node v1
mapDirSection f gr =
   FlowTopo.Section {
      label = fmap f $ label gr,
      topology = mapDirTopology f $ topology gr
   }

mapTopology ::
   (v0 -> v1) ->
   Topology node v0 -> Topology node v1
mapTopology f gr =
   Graph.mapNode (mapSums f) $
   Graph.mapEdge (fmap $ fmap f) gr

mapDirTopology ::
   (v0 -> v1) ->
   DirTopology node v0 -> DirTopology node v1
mapDirTopology f gr =
   Graph.mapNode (mapSums f) $
   Graph.mapEdge (fmap f) gr


checkedZipWithSection ::
   (Ord node) =>
   Caller ->
   (v0 -> v1 -> v2) ->
   Section node v0 ->
   Section node v1 ->
   Section node v2
checkedZipWithSection caller f gr0 gr1 =
   FlowTopo.Section {
      label = liftA2 f (label gr0) (label gr1),
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
   liftA2 FlowTopo.Section (traverse f lab) (traverseTopology f topo)

traverseDirSection ::
   (Applicative f, Ord node) =>
   (v0 -> f v1) ->
   DirSection node v0 -> f (DirSection node v1)
traverseDirSection f (FlowTopo.Section lab topo) =
   liftA2 FlowTopo.Section (traverse f lab) (traverseDirTopology f topo)

traverseTopology ::
   (Applicative f, Ord node) =>
   (v0 -> f v1) ->
   Topology node v0 -> f (Topology node v1)
traverseTopology f =
   Graph.traverse (traverseSums f) (traverse $ traverse f)

traverseDirTopology ::
   (Applicative f, Ord node) =>
   (v0 -> f v1) ->
   DirTopology node v0 -> f (DirTopology node v1)
traverseDirTopology f =
   Graph.traverse (traverseSums f) (traverse f)


toAssignMap ::
   (Node.C node) =>
   Section node v -> AssignMap node v
toAssignMap =
   fold . mapSectionWithVar AssignMap.singleton


lookupPower ::
   (Ord node) => Idx.Power node -> Section node v -> Maybe v
lookupPower =
   lookupAutoDirSection flowPowerOut flowPowerIn (\(Idx.Power se) -> se)

lookupEnergy ::
   (Ord node) => Idx.Energy node -> Section node v -> Maybe v
lookupEnergy =
   lookupAutoDirSection flowEnergyOut flowEnergyIn (\(Idx.Energy se) -> se)

lookupX ::
   (Ord node) => Idx.X node -> Section node v -> Maybe v
lookupX =
   lookupAutoDirSection flowXOut flowXIn (\(Idx.X se) -> se)

lookupAutoDirSection ::
   Ord node =>
   (Flow v -> x) ->
   (Flow v -> x) ->
   (idx -> Idx.Position node) ->
   idx -> Section node v -> Maybe x
lookupAutoDirSection fieldOut fieldIn unpackIdx idx =
   lookupAutoDir fieldOut fieldIn unpackIdx idx . FlowTopo.topology

lookupAutoDir ::
   Ord node =>
   (Flow v -> x) ->
   (Flow v -> x) ->
   (idx -> Idx.Position node) ->
   idx -> Topology node v -> Maybe x
lookupAutoDir fieldOut fieldIn unpackIdx idx topo =
   case unpackIdx idx of
      e ->
         mplus
            (FlowTopo.lookupEdge fieldOut (Topo.dirEdgeFromOutPos e) topo)
            (FlowTopo.lookupEdge fieldIn  (Topo.dirEdgeFromInPos  e) topo)


lookupEta :: (Ord node) => Idx.Eta node -> Section node v -> Maybe v
lookupEta (Idx.Eta e) =
   FlowTopo.lookupEdge flowEta (Topo.dirEdgeFromOutPos e) . FlowTopo.topology

lookupSum :: (Ord node) => Idx.Sum node -> Section node v -> Maybe v
lookupSum (Idx.Sum dir node) s = do
   sums <- lookupSums node s
   case dir of
      Idx.In  -> sumIn sums
      Idx.Out -> sumOut sums

lookupSums :: (Ord node) => node -> Section node v -> Maybe (Sums v)
lookupSums node = Graph.lookupNode node . FlowTopo.topology

lookupDTime :: Idx.DTime node -> Section node v -> Maybe v
lookupDTime Idx.DTime = Just . dtime . FlowTopo.label

lookupOne :: Idx.One node -> Section node v -> Maybe v
lookupOne Idx.One = Just . one . FlowTopo.label


class (Var.Index idx, Var.FormatIndex idx) => Lookup idx where
   lookup :: (Ord node) => idx node -> Section node v -> Maybe v

instance Lookup Idx.Energy where
   lookup = lookupEnergy

instance Lookup Idx.Power where
   lookup = lookupPower

instance Lookup Idx.Eta where
   lookup = lookupEta

instance Lookup Idx.DTime where
   lookup = lookupDTime

instance Lookup Idx.One where
   lookup = lookupOne

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
      label = mapLabelWithVar f $ label gr,
      topology = mapTopologyWithVar f $ topology gr
   }

mapLabelWithVar ::
   (Var.Signal node -> v0 -> v1) ->
   Label v0 -> Label v1
mapLabelWithVar f =
   liftA2 f labelVars

labelVars :: Label (Var.Signal node)
labelVars =
   Label {
      dtime = Var.DTime Idx.DTime,
      one = Var.One Idx.One
   }

mapTopologyWithVar ::
   (Ord node) =>
   (Var.Signal node -> v0 -> v1) ->
   Topology node v0 ->
   Topology node v1
mapTopologyWithVar f topo =
   Graph.mapNodeWithKey (mapSumsWithVar f) $
   Graph.mapEdgeWithKey (FlowTopo.liftEdgeFlow $ mapFlowWithVar f) topo

mapSumsWithVar ::
   (Var.Signal node -> v0 -> v1) -> node -> Sums v0 -> Sums v1
mapSumsWithVar f n (Sums {sumIn = sin, sumOut = sout}) =
   Sums {
      sumIn = flip fmap sin $ f (Var.Sum $ Idx.Sum Idx.In n),
      sumOut = flip fmap sout $ f (Var.Sum $ Idx.Sum Idx.Out n)
   }

mapFlowWithVar ::
   (Var.Signal node -> v0 -> v1) ->
   Graph.DirEdge node -> Flow v0 -> Flow v1
mapFlowWithVar f e =
   liftA2 f (flowVars <*> pure e)


sectionFromPlain ::
   (Ord node, Unknown v) =>
   Topo.FlowTopology node -> Section node v
sectionFromPlain gr =
   FlowTopo.Section (pure unknown) $
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


data Label v =
   Label { dtime, one :: v }
   deriving (Eq)

data Flow v =
   Flow {
      flowXOut, flowPowerOut, flowEnergyOut,
      flowEta,
      flowEnergyIn, flowPowerIn, flowXIn :: v
   }
   deriving (Eq)

data Sums v =
   Sums { sumIn, sumOut :: Maybe v }
   deriving (Eq)


instance Functor Flow where
   fmap f (Flow xout pout eout eta ein pin xin) =
      Flow (f xout) (f pout) (f eout) (f eta) (f ein) (f pin) (f xin)

instance Foldable Flow where
   foldMap = foldMapDefault

instance Traversable Flow where
   traverse f (Flow xout pout eout eta ein pin xin) =
      pure Flow <*> f xout <*> f pout <*> f eout <*> f eta <*> f ein <*> f pin <*> f xin

instance Applicative Flow where
   pure a = Flow a a a a a a a
   Flow fxout fpout feout feta fein fpin fxin
         <*> Flow xout pout eout eta ein pin xin =
      Flow
         (fxout xout) (fpout pout) (feout eout)
         (feta eta) (fein ein) (fpin pin) (fxin xin)


instance Functor Label where
   fmap f (Label dt o) =
      Label (f dt) (f o)

instance Foldable Label where
   foldMap = foldMapDefault

instance Traversable Label where
   traverse f (Label dt o) =
      pure Label <*> f dt <*> f o

instance Applicative Label where
   pure a = Label a a
   Label fdt fo <*> Label dt o =
      Label (fdt dt) (fo o)


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


flowVars :: Flow (Graph.DirEdge node -> Var.Signal node)
flowVars =
   Flow {
      flowPowerOut = Var.index . Idx.Power . Topo.outPosFromDirEdge,
      flowPowerIn = Var.index . Idx.Power . Topo.inPosFromDirEdge,
      flowEnergyOut = Var.index . Idx.Energy . Topo.outPosFromDirEdge,
      flowEnergyIn = Var.index . Idx.Energy . Topo.inPosFromDirEdge,
      flowXOut = Var.index . Idx.X . Topo.outPosFromDirEdge,
      flowXIn = Var.index . Idx.X . Topo.inPosFromDirEdge,
      flowEta = Var.index . Idx.Eta . Topo.outPosFromDirEdge
   }


dirFromSums :: Sums v -> Maybe Topo.StoreDir
dirFromSums sums =
   case (sumIn sums, sumOut sums) of
      (Nothing, Nothing) -> Nothing
      (Just _, Nothing) -> Just Topo.In
      (Nothing, Just _) -> Just Topo.Out
      (Just _, Just _) -> error "storage cannot be both In and Out"

sumsFromDir :: v -> Maybe Topo.StoreDir -> Sums v
sumsFromDir x mdir =
   case mdir of
      Nothing       -> Sums {sumIn = Nothing, sumOut = Nothing}
      Just Topo.In  -> Sums {sumIn = Just x,  sumOut = Nothing}
      Just Topo.Out -> Sums {sumIn = Nothing, sumOut = Just x}

foldMapDir ::
   (Node.C node, Monoid w) =>
   (v -> w) -> DirSection node v -> w
foldMapDir fv =
   foldDir . mapDirSection fv

foldDir ::
   (Node.C node, Monoid w) =>
   DirSection node w -> w
foldDir = MW.execWriter . traverseDirSection MW.tell


foldMap ::
   (Node.C node, Monoid w) =>
   (v -> w) -> Section node v -> w
foldMap fv =
   fold . mapSection fv

fold ::
   (Node.C node, Monoid w) =>
   Section node w -> w
fold = MW.execWriter . traverseSection MW.tell
