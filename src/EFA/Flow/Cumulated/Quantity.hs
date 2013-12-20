{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.Cumulated.Quantity (
   Graph, CumGraph,
   Sums(..), Cum(..), Flow(..),

   mapGraph,
   traverseGraph,

   mapGraphWithVar,
   mapCumGraphWithVar,

   mapSumsWithVar,
   mapFlowWithVar,
   mapCumWithVar,

   sumsVars,
   flowVars,
   cumVars,

   fromSequenceFlow,
   fromSequenceFlowResult,
   fromSequenceFlowRecordResult,

   flowResultFromCum,
   flowResultFromCumResult,

   toAssignMap,

   lookupPower,
   lookupEnergy,
   lookupX,
   lookupEta,
   lookupDTime,
   lookupSum,

   Lookup, lookup,

   fold, foldMap,
   ) where

import qualified EFA.Flow.Cumulated.Variable as CumVar
import qualified EFA.Flow.Cumulated.AssignMap as AssignMap
import qualified EFA.Flow.Cumulated.Index as CumIdx
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology as FlowTopoPlain
import EFA.Flow.Cumulated.AssignMap (AssignMap)
import EFA.Flow.Topology.Quantity (Sums(..))

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph as Graph

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+))
import EFA.Equation.Unknown (Unknown(unknown))
import EFA.Equation.Result (Result(Determined))

import qualified EFA.Utility.Map as MapU
import qualified Data.Map as Map

import qualified Control.Monad.Trans.Writer as MW
import Control.Applicative (Applicative, pure, liftA2, liftA3, (<*>), (<|>))
import Control.Monad ((<=<))

import qualified Data.Foldable as Fold
import Data.Traversable (Traversable, traverse, foldMapDefault)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid)

import Prelude hiding (lookup, init, seq, sequence, sin, sum)


type
   Graph node a =
      Graph.Graph node Graph.DirEdge (Sums a) (Flow a)

data Flow a =
   Flow {
      flowDTime,
      flowXOut, flowPowerOut, flowEnergyOut,
      flowEta,
      flowEnergyIn, flowPowerIn, flowXIn :: a
   } deriving (Eq)


instance Functor Flow where
   fmap f (Flow time xout pout eout eta ein pin xin) =
      Flow (f time) (f xout) (f pout) (f eout) (f eta) (f ein) (f pin) (f xin)

instance Foldable Flow where
   foldMap = foldMapDefault

instance Traversable Flow where
   traverse f (Flow time xout pout eout eta ein pin xin) =
      pure Flow <*> f time
         <*> f xout <*> f pout <*> f eout
         <*> f eta <*> f ein <*> f pin <*> f xin

instance Applicative Flow where
   pure a = Flow a a a a a a a a
   Flow ftime fxout fpout feout feta fein fpin fxin
         <*> Flow time xout pout eout eta ein pin xin =
      Flow (ftime time)
         (fxout xout) (fpout pout) (feout eout)
         (feta eta) (fein ein) (fpin pin) (fxin xin)


mapGraph ::
   (a0 -> a1) ->
   Graph node a0 -> Graph node a1
mapGraph f =
   Graph.mapEdge (fmap f) . Graph.mapNode (fmap f)


traverseGraph ::
   (Applicative f, Ord node) =>
   (a0 -> f a1) ->
   Graph node a0 -> f (Graph node a1)
traverseGraph f =
   Graph.traverse (traverse f) (traverse f)



type
   CumGraph node a =
      Graph.Graph node Graph.DirEdge (Sums a) (Cum a)

data Cum a =
   Cum {
      cumDTime, cumEnergyOut, cumEnergyIn :: a
   }

instance Functor Cum where
   fmap f (Cum time eout ein) = Cum (f time) (f eout) (f ein)

instance Applicative Cum where
   pure a = Cum a a a
   Cum ftime feout fein <*> Cum time eout ein =
      Cum (ftime time) (feout eout) (fein ein)

instance Foldable Cum where
   foldMap = foldMapDefault

instance Traversable Cum where
   traverse f (Cum dt eout ein) =
      liftA3 Cum (f dt) (f eout) (f ein)

cumFromFlow :: a -> SeqFlow.Flow a -> Cum a
cumFromFlow time flow =
   Cum time
      (SeqFlow.flowEnergyOut flow)
      (SeqFlow.flowEnergyIn flow)

cumFromFlowGraph ::
   (Ord node) =>
   FlowTopo.Section node a -> CumGraph node a
cumFromFlowGraph (FlowTopoPlain.Section time topo) =
   Graph.mapEdge (cumFromFlow time) $
   FlowTopoPlain.dirFromFlowGraph topo

flowResultFromCum :: Cum a -> Flow (Result a)
flowResultFromCum =
   flowResultFromCumResult . fmap Determined

flowResultFromCumResult :: (Unknown a) => Cum a -> Flow a
flowResultFromCumResult cum =
   (pure unknown) {
      flowEnergyOut = cumEnergyOut cum,
      flowEnergyIn  = cumEnergyIn  cum,
      flowDTime     = cumDTime cum
   }


fromSequenceFlowGen ::
   (Ord node) =>
   (v -> a) ->
   (a -> a -> a) ->
   SeqFlow.Sequence node v ->
   CumGraph node a
fromSequenceFlowGen integrate add =
   foldl1 (addCumGraph add) . Map.elems .
   fmap (cumFromFlowGraph . snd) .
   SeqFlow.mapSequence integrate

addCumGraph ::
   (Ord node) =>
   (a -> a -> a) ->
   CumGraph node a -> CumGraph node a -> CumGraph node a
addCumGraph add x y =
   Graph.fromMap
      (MapU.checkedZipWith "addCumGraph"
         (addSums add)
         (Graph.nodeLabels x) (Graph.nodeLabels y))
      (Map.unionWith (liftA2 add)
         (Graph.edgeLabels x) (Graph.edgeLabels y))

addSums :: (a -> a -> a) -> Sums a -> Sums a -> Sums a
addSums add (Sums i0 o0) (Sums i1 o1) =
   Sums (addMaybe add i0 i1) (addMaybe add o0 o1)

addMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
addMaybe add x y = liftA2 add x y <|> x <|> y


lookupPower ::
   (Ord node) => CumIdx.Power node -> Graph node a -> Maybe a
lookupPower (CumIdx.Power d se) =
   lookupAutoDir flowPowerOut flowPowerIn d se

lookupEnergy ::
   (Ord node) => CumIdx.Energy node -> Graph node a -> Maybe a
lookupEnergy (CumIdx.Energy d se) =
   lookupAutoDir flowEnergyOut flowEnergyIn d se

lookupX ::
   (Ord node) => CumIdx.X node -> Graph node a -> Maybe a
lookupX (CumIdx.X d se) =
   lookupAutoDir flowXOut flowXIn d se

lookupAutoDir ::
   (Ord node) =>
   (Flow a -> a) ->
   (Flow a -> a) ->
   CumIdx.Direction ->
   CumIdx.DirEdge node ->
   Graph node a -> Maybe a
lookupAutoDir fieldOut fieldIn dir =
   lookupEdge
      (case dir of
         CumIdx.Out -> fieldOut
         CumIdx.In  -> fieldIn)


lookupEta :: (Ord node) => CumIdx.Eta node -> Graph node a -> Maybe a
lookupEta (CumIdx.Eta se) = lookupEdge flowEta se

lookupDTime :: (Ord node) => CumIdx.DTime node -> Graph node a -> Maybe a
lookupDTime (CumIdx.DTime se) = lookupEdge flowDTime se

lookupEdge ::
   Ord n =>
   (el -> a) ->
   CumIdx.DirEdge n ->
   Graph.Graph n Graph.DirEdge nl el ->
   Maybe a
lookupEdge f e =
   fmap f . Graph.lookupEdge e


lookupSum :: (Ord node) => CumIdx.Sum node -> Graph node a -> Maybe a
lookupSum (CumIdx.Sum dir node) =
   (case dir of
      CumIdx.In  -> sumIn
      CumIdx.Out -> sumOut)
   <=<
   Graph.lookupNode node


class (CumVar.Index idx, CumVar.FormatIndex idx) => Lookup idx where
   lookup ::
      (Ord node) => idx node -> Graph node a -> Maybe a

instance Lookup CumIdx.Energy where
   lookup = lookupEnergy

instance Lookup CumIdx.Power where
   lookup = lookupPower

instance Lookup CumIdx.Eta where
   lookup = lookupEta

instance Lookup CumIdx.DTime where
   lookup = lookupDTime

instance Lookup CumIdx.X where
   lookup = lookupX

instance Lookup CumIdx.Sum where
   lookup = lookupSum


fromSequenceFlow ::
   (Ord node, Arith.Constant a, a ~ Arith.Scalar v, Arith.Integrate v) =>
   SeqFlow.Sequence node v ->
   CumGraph node a
fromSequenceFlow =
   fromSequenceFlowGen Arith.integrate (~+)

fromSequenceFlowResult ::
   (Ord node, Arith.Constant a, a ~ Arith.Scalar v, Arith.Integrate v) =>
   SeqFlow.Sequence node (Result v) ->
   CumGraph node (Result a)
fromSequenceFlowResult =
   fromSequenceFlowGen (fmap Arith.integrate) (liftA2 (~+))

fromSequenceFlowRecordResult ::
   (Ord node, Arith.Constant a, a ~ Arith.Scalar v, Arith.Integrate v,
    Applicative rec) =>
   SeqFlow.Sequence node (rec (Result v)) ->
   CumGraph node (rec (Result a))
fromSequenceFlowRecordResult =
   fromSequenceFlowGen (fmap $ fmap Arith.integrate) (liftA2 $ liftA2 (~+))


mapGraphWithVar ::
   (Ord node) =>
   (CumVar.Any node -> a0 -> a1) ->
   Graph node a0 ->
   Graph node a1
mapGraphWithVar f =
   Graph.mapNodeWithKey (mapSumsWithVar f)
   .
   Graph.mapEdgeWithKey (mapFlowWithVar f)

mapCumGraphWithVar ::
   (Ord node) =>
   (CumVar.Any node -> a0 -> a1) ->
   CumGraph node a0 ->
   CumGraph node a1
mapCumGraphWithVar f =
   Graph.mapNodeWithKey (mapSumsWithVar f)
   .
   Graph.mapEdgeWithKey (mapCumWithVar f)

mapSumsWithVar ::
   (CumVar.Any node -> a0 -> a1) ->
   node -> Sums a0 -> Sums a1
mapSumsWithVar f n = liftA2 f (sumsVars <*> pure n)

mapFlowWithVar ::
   (CumVar.Any node -> a0 -> a1) ->
   Graph.DirEdge node -> Flow a0 -> Flow a1
mapFlowWithVar f e = liftA2 f (flowVars <*> pure e)

mapCumWithVar ::
   (CumVar.Any node -> a0 -> a1) ->
   Graph.DirEdge node -> Cum a0 -> Cum a1
mapCumWithVar f e = liftA2 f (cumVars <*> pure e)

sumsVars :: Sums (node -> CumVar.Any node)
sumsVars =
   Sums {
      sumOut = Just $ CumVar.index . CumIdx.Sum CumIdx.Out,
      sumIn  = Just $ CumVar.index . CumIdx.Sum CumIdx.In
   }

flowVars :: Flow (Graph.DirEdge node -> CumVar.Any node)
flowVars =
   Flow {
      flowPowerOut = CumVar.index . CumIdx.Power CumIdx.Out,
      flowPowerIn = CumVar.index . CumIdx.Power CumIdx.In,
      flowEnergyOut = CumVar.index . CumIdx.Energy CumIdx.Out,
      flowEnergyIn = CumVar.index . CumIdx.Energy CumIdx.In,
      flowXOut = CumVar.index . CumIdx.X CumIdx.Out,
      flowXIn = CumVar.index . CumIdx.X CumIdx.In,
      flowEta = CumVar.index . CumIdx.Eta,
      flowDTime = CumVar.index . CumIdx.DTime
   }

cumVars :: Cum (Graph.DirEdge node -> CumVar.Any node)
cumVars =
   Cum {
      cumEnergyOut = CumVar.index . CumIdx.Energy CumIdx.Out,
      cumEnergyIn = CumVar.index . CumIdx.Energy CumIdx.In,
      cumDTime = CumVar.index . CumIdx.DTime
   }


toAssignMap ::
   (Node.C node) =>
   Graph node a -> AssignMap node a
toAssignMap =
   fold . mapGraphWithVar AssignMap.singleton


foldMap ::
   (Node.C node, Monoid w) =>
   (v -> w) -> Graph node v -> w
foldMap f =
   fold . mapGraph f

fold ::
   (Node.C node, Monoid w) =>
   Graph node w -> w
fold = MW.execWriter . traverseGraph MW.tell
