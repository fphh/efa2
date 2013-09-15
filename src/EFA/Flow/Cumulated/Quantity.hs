{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.Cumulated.Quantity (
   Graph,
   Sums(..), Cum(..), Flow(..),

   mapGraph,
   traverseGraph,
   mapGraphWithVar,

   fromSequenceFlow,
   fromSequenceFlowResult,

   flowResultFromCum,
   flowResultFromCumResult,

   lookupPower,
   lookupEnergy,
   lookupX,
   lookupEta,
   lookupDTime,
   lookupSum,

   Lookup, lookup,
   ) where

import qualified EFA.Flow.Cumulated.Variable as CumVar
import qualified EFA.Flow.Cumulated.Index as CumIdx
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Quantity as Quant

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+))
import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import qualified EFA.Utility.Map as MapU

import qualified Data.Map as Map

import Control.Applicative (Applicative, pure, liftA2, (<*>), (<|>))
import Control.Monad ((<=<))
import Data.Traversable (Traversable, traverse, foldMapDefault)
import Data.Foldable (Foldable, foldMap)

import Prelude hiding (lookup, init, seq, sequence, sin, sum)


type
   Graph node a =
      Graph.Graph node Graph.DirEdge (Sums a) (Flow a)

data Sums a =
   Sums { sumIn, sumOut :: Maybe a }

data Flow a =
   Flow {
      flowDTime,
      flowPowerOut, flowEnergyOut, flowXOut,
      flowEta,
      flowXIn, flowEnergyIn, flowPowerIn :: a
   }


instance Functor Sums where
   fmap f (Sums i o) = Sums (fmap f i) (fmap f o)

instance Foldable Sums where
   foldMap = foldMapDefault

instance Traversable Sums where
   traverse f (Sums i o) = liftA2 Sums (traverse f i) (traverse f o)

instance Applicative Sums where
   pure a = Sums (Just a) (Just a)
   (Sums fi fo) <*> (Sums i o) = Sums (fi <*> i) (fo <*> o)


instance Functor Flow where
   fmap f (Flow time pout eout xout eta xin ein pin) =
      Flow (f time) (f pout) (f eout) (f xout) (f eta) (f xin) (f ein) (f pin)

instance Foldable Flow where
   foldMap = foldMapDefault

instance Traversable Flow where
   traverse f (Flow time pout eout xout eta xin ein pin) =
      pure Flow <*> f time
         <*> f pout <*> f eout <*> f xout
         <*> f eta <*> f xin <*> f ein <*> f pin

instance Applicative Flow where
   pure a = Flow a a a a a a a a
   Flow ftime fpout feout fxout feta fxin fein fpin
         <*> Flow time pout eout xout eta xin ein pin =
      Flow (ftime time)
         (fpout pout) (feout eout) (fxout xout)
         (feta eta) (fxin xin) (fein ein) (fpin pin)


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

cumFromFlow :: a -> SeqFlow.Flow a -> Cum a
cumFromFlow time flow =
   Cum time
      (SeqFlow.flowEnergyOut flow)
      (SeqFlow.flowEnergyIn flow)

cumFromFlowGraph ::
   (Ord node) =>
   a -> SeqFlow.Topology node a a -> CumGraph node a
cumFromFlowGraph time =
   Graph.mapNode
      (\(SeqFlow.Sums i o) ->
         Sums (fmap SeqFlow.flowSum i) (fmap SeqFlow.flowSum o)) .
   Graph.mapEdge (cumFromFlow time) .
   Quant.dirFromFlowGraph

flowResultFromCum :: Cum a -> Flow (Result a)
flowResultFromCum =
   flowResultFromCumResult . fmap Determined

flowResultFromCumResult :: Cum (Result a) -> Flow (Result a)
flowResultFromCumResult cum =
   (pure Undetermined) {
      flowEnergyOut = cumEnergyOut cum,
      flowEnergyIn  = cumEnergyIn  cum,
      flowDTime     = cumDTime cum
   }


fromSequenceFlowGen ::
   (Ord node) =>
   (v -> a) ->
   (a -> a -> a) ->
   SeqFlow.Sequence node a v ->
   CumGraph node a
fromSequenceFlowGen integrate add =
   foldl1 (addCumGraph add) . Map.elems .
   fmap (uncurry cumFromFlowGraph . snd) .
   SeqFlow.mapSequence id integrate

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
   lookupStruct flowPowerOut flowPowerIn d se

lookupEnergy ::
   (Ord node) => CumIdx.Energy node -> Graph node a -> Maybe a
lookupEnergy (CumIdx.Energy d se) =
   lookupStruct flowEnergyOut flowEnergyIn d se

lookupX ::
   (Ord node) => CumIdx.X node -> Graph node a -> Maybe a
lookupX (CumIdx.X d se) =
   lookupStruct flowXOut flowXIn d se

lookupStruct ::
   (Ord node) =>
   (Flow a -> a) ->
   (Flow a -> a) ->
   CumIdx.Direction ->
   CumIdx.StructureEdge node ->
   Graph node a -> Maybe a
lookupStruct fieldOut fieldIn dir =
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
   CumIdx.StructureEdge n ->
   Graph.Graph n Graph.DirEdge nl el ->
   Maybe a
lookupEdge f se =
   fmap f . Graph.lookupEdge (Topo.dirEdgeFromStructureEdge se)


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
   SeqFlow.Sequence node a v ->
   CumGraph node a
fromSequenceFlow =
   fromSequenceFlowGen Arith.integrate (~+)

fromSequenceFlowResult ::
   (Ord node, Arith.Constant a, a ~ Arith.Scalar v, Arith.Integrate v) =>
   SeqFlow.Sequence node (Result a) (Result v) ->
   CumGraph node (Result a)
fromSequenceFlowResult =
   fromSequenceFlowGen (fmap Arith.integrate) (liftA2 (~+))


mapGraphWithVar ::
   (Ord node) =>
   (CumVar.Any node -> a0 -> a1) ->
   Graph node a0 ->
   Graph node a1
mapGraphWithVar f =
   Graph.mapNodeWithKey
      (\n -> liftA2 f (sumsVars <*> pure n))
   .
   Graph.mapEdgeWithKey
      (\e ->
         liftA2 f
            (flowVars <*> pure (Topo.structureEdgeFromDirEdge e)))


sumsVars :: Sums (node -> CumVar.Any node)
sumsVars =
   Sums {
      sumOut = Just $ CumVar.index . CumIdx.Sum CumIdx.Out,
      sumIn  = Just $ CumVar.index . CumIdx.Sum CumIdx.In
   }

flowVars :: Flow (CumIdx.StructureEdge node -> CumVar.Any node)
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
