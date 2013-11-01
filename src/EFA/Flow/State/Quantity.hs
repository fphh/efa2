{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module EFA.Flow.State.Quantity (
   Graph, StateFlow.states, StateFlow.storages,
   Topology, States, Storages,
   Sums(..), Carry(..), Flow(..),
   CumGraph, Cum(..),

   mapGraph,
   mapStorages,
   mapStates,

   checkedZipWithGraph,
   checkedZipWithStates,
   checkedZipWithStorages,

   traverseGraph,
   traverseStorages,
   traverseStates,

   mapGraphWithVar,
   mapStoragesWithVar,
   mapStatesWithVar,

   fromSequenceFlow,
   fromSequenceFlowResult,
   fromStatesAndSequenceFlow,
   fromStatesAndSequenceFlowResult,

   cumFromFlow,
   flowResultFromCum,
   flowResultFromCumResult,

   graphFromCumResult,
   graphFromStates,
   graphFromStateMap,

   lookupSums,

   Lookup, lookup,
   LookupScalar, lookupScalar,
   LookupSignal, lookupSignal,
   Env.TypeOf, Env.Element, Env.switchPart,
   ) where

import qualified EFA.Flow.State as StateFlow
import qualified EFA.Flow.SequenceState.Quantity as Env
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Storage.Quantity as StorageQuant
import qualified EFA.Flow.Storage.Variable as StorageVar
import qualified EFA.Flow.Storage.Index as StorageIdx
import qualified EFA.Flow.Storage as Storage
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Variable as TopoVar
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Flow.Part.Map as PartMap
import EFA.Flow.Part.Map (PartMap)
import EFA.Flow.State (states, storages)
import EFA.Flow.Topology.Quantity (Sums(..), Flow(..))

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Flow.SequenceState.Variable as Var
import EFA.Equation.Unknown (Unknown)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import EFA.Equation.Arithmetic ((~+))
import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified EFA.Signal.Sequence as Sequ

import qualified EFA.Utility.Map as MapU
import EFA.Utility.Map (Caller)

import qualified Control.Monad.Trans.State as MS

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Stream as Stream; import Data.Stream (Stream)

import Control.Applicative (Applicative, pure, liftA2, (<*>))
import Control.Monad ((<=<))
import Data.Traversable (Traversable, traverse, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Maybe (fromMaybe)

import Prelude hiding (lookup, init, seq, sequence, sin, sum)



type
   Storages node a = StateFlow.Storages node a (Carry a)

type
   States node v =
      StateFlow.States node Graph.EitherEdge v (Sums v) (Maybe (Flow v))

type
   Graph node a v =
      StateFlow.Graph node Graph.EitherEdge
         v (Sums v) a (Maybe (Flow v)) (Carry a)

data Carry a =
   Carry {
      carryEnergy, carryXOut, carryXIn :: a
   }


instance StorageQuant.Carry Carry where
   carryEnergy = carryEnergy
   carryXOut   = carryXOut
   carryXIn    = carryXIn

   type CarryPart Carry = Idx.State
   carryVars =
      Carry {
         carryEnergy = StorageVar.index . StorageIdx.Energy,
         carryXOut = StorageVar.index . StorageIdx.X . StorageIdx.bondFromEdge,
         carryXIn = StorageVar.index . StorageIdx.X . StorageIdx.flip . StorageIdx.bondFromEdge
      }


instance Functor Carry where
   fmap f (Carry e xout xin) =
      Carry (f e) (f xout) (f xin)


instance Foldable Carry where
   foldMap = foldMapDefault


instance Traversable Carry where
   traverse f (Carry e xout xin) =
      pure Carry <*> f e <*> f xout <*> f xin


instance Applicative Carry where
   pure a = Carry a a a
   Carry fe fxout fxin <*> Carry e xout xin =
      Carry (fe e) (fxout xout) (fxin xin)


mapGraph ::
   (a0 -> a1) ->
   (v0 -> v1) ->
   Graph node a0 v0 -> Graph node a1 v1
mapGraph f g gr =
   StateFlow.Graph {
      states   = mapStates   g $ states gr,
      storages = mapStorages f $ storages gr
   }

mapStates ::
   (v0 -> v1) ->
   States node v0 -> States node v1
mapStates f =
   fmap (FlowTopo.mapSection f)

mapStorages ::
   (a0 -> a1) ->
   Storages node a0 -> Storages node a1
mapStorages f =
   fmap (StorageQuant.mapGraph f)


checkedZipWithGraph ::
   (Ord node) =>
   Caller ->
   (a0 -> a1 -> a2) ->
   (v0 -> v1 -> v2) ->
   Graph node a0 v0 ->
   Graph node a1 v1 ->
   Graph node a2 v2
checkedZipWithGraph caller f g gr0 gr1 =
   StateFlow.Graph {
      states   = checkedZipWithStates   caller g (states   gr0) (states   gr1),
      storages = checkedZipWithStorages caller f (storages gr0) (storages gr1)
   }

checkedZipWithStates ::
   (Ord node) =>
   Caller ->
   (v0 -> v1 -> v2) ->
   States node v0 ->
   States node v1 ->
   States node v2
checkedZipWithStates caller f =
   MapU.checkedZipWith (caller++".checkedZipWithStates")
      (\gr0 gr1 ->
         FlowTopo.checkedZipWithSection
            (caller++".checkedZipWithStates.section")
            f gr0 gr1)

checkedZipWithStorages ::
   (Ord node) =>
   Caller ->
   (a0 -> a1 -> a2) ->
   Storages node a0 ->
   Storages node a1 ->
   Storages node a2
checkedZipWithStorages caller f =
   let name = caller++".checkedZipWithStorages"
   in  MapU.checkedZipWith name
          (\graph0 graph1 ->
             Storage.checkedZipWith name f (liftA2 f) graph0 graph1)


traverseGraph ::
   (Applicative f, Ord node) =>
   (a0 -> f a1) ->
   (v0 -> f v1) ->
   Graph node a0 v0 -> f (Graph node a1 v1)
traverseGraph f g (StateFlow.Graph sts seq) =
   liftA2 StateFlow.Graph
      (traverseStorages f sts)
      (traverseStates   g seq)

traverseStates ::
   (Applicative f, Ord node) =>
   (v0 -> f v1) ->
   States node v0 -> f (States node v1)
traverseStates f =
   traverse (FlowTopo.traverseSection f)

traverseStorages ::
   (Applicative f, Ord node) =>
   (a0 -> f a1) ->
   Storages node a0 -> f (Storages node a1)
traverseStorages f =
   traverse (StorageQuant.traverseGraph f)



type Topology node nodeLabel = Graph.Graph node Graph.EitherEdge nodeLabel ()

_states ::
   (Ord node, Ord nodeLabel) =>
   Sequ.List (Topology node nodeLabel) ->
   Map (Topology node nodeLabel) Idx.State
_states =
   Map.fromAscList .
   flip zip [Idx.state0 ..] .
   Set.toAscList . foldMap Set.singleton


identify ::
   (Ord k) =>
   k -> MS.State (Map k i, Stream i) i
identify k = do
   (m,it) <- MS.get
   case Map.lookup k m of
      Just i -> return i
      Nothing ->
         case it of
            Stream.Cons i is -> do
               MS.put (Map.insert k i m, is)
               return i

stateMap ::
   (Ord node, Ord nodeLabel) =>
   Map Idx.Section (Topology node nodeLabel) ->
   Map Idx.Section Idx.State
stateMap =
   flip MS.evalState (Map.empty, Stream.iterate succ $ Idx.state0) .
   traverse identify

unitLabels ::
   FlowTopoPlain.Section n e sl nl el -> Graph.Graph n e () ()
unitLabels =
   Graph.mapEdge (const ()) .
   Graph.mapNode (const ()) .
   FlowTopo.topology

stateMapFromSequence ::
   (Ord node) =>
   SeqFlow.Sequence node v -> Map Idx.Section Idx.State
stateMapFromSequence =
   stateMap . fmap (unitLabels . snd)

stateMapFromStatesAndSequence ::
   (Ord node) =>
   States node v0 ->
   SeqFlow.Sequence node v1 ->
   Map Idx.Section Idx.State
stateMapFromStatesAndSequence sts seq =
   MapU.compose
      (MapU.reverse (fmap unitLabels sts))
      (fmap (unitLabels . snd) seq)


type
   CumGraph node a =
      StateFlow.Graph node Graph.EitherEdge a (Sums a) a (Maybe (Cum a)) a



{- |
If allStEdges
  Then: Insert all possible storage edges.
  Else: Insert only the storage edges that have counterparts in the sequence flow graph.
-}
fromSequenceFlow ::
   (Ord node, Arith.Constant a, a ~ Arith.Scalar v, Arith.Integrate v) =>
   Bool ->
   SeqFlow.Graph node a v ->
   CumGraph node a
fromSequenceFlow allStEdges gr =
   fromSequenceFlowGen Arith.integrate (~+) Arith.zero
      allStEdges (stateMapFromSequence $ SeqFlow.sequence gr) gr

fromSequenceFlowResult ::
   (Ord node, Arith.Constant a, a ~ Arith.Scalar v, Arith.Integrate v) =>
   Bool ->
   SeqFlow.Graph node (Result a) (Result v) ->
   CumGraph node (Result a)
fromSequenceFlowResult allStEdges gr =
   fromSequenceFlowGen (fmap Arith.integrate) (liftA2 (~+)) (pure Arith.zero)
      allStEdges (stateMapFromSequence $ SeqFlow.sequence gr) gr


fromStatesAndSequenceFlow ::
   (Ord node, Arith.Constant a, a ~ Arith.Scalar v, Arith.Integrate v) =>
   Bool ->
   States node v0 ->
   SeqFlow.Graph node a v ->
   CumGraph node a
fromStatesAndSequenceFlow allStEdges sts gr =
   fromSequenceFlowGen Arith.integrate (~+) Arith.zero
      allStEdges (stateMapFromStatesAndSequence sts $ SeqFlow.sequence gr) gr

fromStatesAndSequenceFlowResult ::
   (Ord node, Arith.Constant a, a ~ Arith.Scalar v, Arith.Integrate v) =>
   Bool ->
   States node v0 ->
   SeqFlow.Graph node (Result a) (Result v) ->
   CumGraph node (Result a)
fromStatesAndSequenceFlowResult allStEdges sts gr =
   fromSequenceFlowGen (fmap Arith.integrate) (liftA2 (~+)) (pure Arith.zero)
      allStEdges (stateMapFromStatesAndSequence sts $ SeqFlow.sequence gr) gr


fromSequenceFlowGen ::
   (Ord node) =>
   (v -> a) ->
   (a -> a -> a) ->
   a ->
   Bool ->
   Map Idx.Section Idx.State ->
   SeqFlow.Graph node a v ->
   CumGraph node a
fromSequenceFlowGen integrate add zero allStEdges secMap gr =
   let sq = SeqFlow.sequence gr
       sts =
          flip cumulateSequence secMap
             (FlowTopoPlain.checkedZipWith "StateFlow.fromSequenceFlow"
                 add (addSums add) (liftA2 (liftA2 add))) $
          fmap (FlowTopoPlain.mapEdge (fmap cumFromFlow) . snd) $
          SeqFlow.mapSequence integrate sq
   in  StateFlow.Graph {
          storages =
             Map.mapWithKey
                (\node (Storage.Graph initExit edges, _) ->
                   Storage.Graph
                      (cumulateSums add secMap initExit)
                      (Map.union
                          (if allStEdges
                             then Map.fromList $
                                  map (flip (,) zero) $
                                  StorageQuant.allEdgesFromSums (sumsMap node sts)
                             else Map.empty) $
                       cumulateCarryEdges add secMap $
                       fmap SeqFlow.carryEnergy edges)) $
             SeqFlow.storages gr,
          states = sts
       }


cumulateCarryEdges ::
   (a -> a -> a) ->
   Map Idx.Section Idx.State ->
   Map (StorageIdx.Edge Idx.Section) a ->
   Map (StorageIdx.Edge Idx.State) a
cumulateCarryEdges add secMap =
   Map.mapKeysWith add
      (mapCarryEdge "cumulateCarryEdges from" secMap)


sumsMap ::
   (Ord node) =>
   node ->
   StateFlow.States node Graph.EitherEdge v (Sums v) (Maybe (Cum v)) ->
   Map Idx.State (Sums v)
sumsMap node =
   fmap (fromMaybe (error "node not in sequence") .
         Graph.lookupNode node . FlowTopo.topology)


data Cum v =
   Cum {
      cumEnergyOut, cumEnergyIn :: v
   }

instance Functor Cum where
   fmap f (Cum eout ein) = Cum (f eout) (f ein)

instance Applicative Cum where
   pure a = Cum a a
   Cum feout fein <*> Cum eout ein =
      Cum (feout eout) (fein ein)

cumFromFlow :: Flow v -> Cum v
cumFromFlow flow =
   Cum
      (SeqFlow.flowEnergyOut flow)
      (SeqFlow.flowEnergyIn flow)

flowResultFromCum :: Cum v -> Flow (Result v)
flowResultFromCum =
   flowResultFromCumResult . fmap Determined

flowResultFromCumResult :: Cum (Result v) -> Flow (Result v)
flowResultFromCumResult cum =
   (pure Undetermined) {
      SeqFlow.flowEnergyOut = cumEnergyOut cum,
      SeqFlow.flowEnergyIn  = cumEnergyIn  cum
   }

carryResultFromResult :: Result a -> Carry (Result a)
carryResultFromResult e =
   (pure Undetermined) {
      carryEnergy = e
   }

graphFromCumResult ::
   CumGraph node (Result a) -> Graph node (Result a) (Result a)
graphFromCumResult gr =
   StateFlow.Graph {
      StateFlow.storages =
         fmap (Storage.mapEdge carryResultFromResult) $
         StateFlow.storages gr,
      StateFlow.states =
         fmap (FlowTopoPlain.mapEdge (fmap flowResultFromCumResult)) $
         StateFlow.states gr
   }


graphFromStates ::
   (Node.C node, Unknown a, Unknown v) =>
   [Topo.FlowTopology node] ->
   Graph node a v
graphFromStates =
   graphFromStateMap . Map.fromList . zip [Idx.state0 ..]

graphFromStateMap ::
   (Node.C node, Unknown a, Unknown v) =>
   Map Idx.State (Topo.FlowTopology node) ->
   Graph node a v
graphFromStateMap flowStates =
   let timeFlowStates = fmap FlowTopo.sectionFromPlain flowStates
   in  StateFlow.Graph {
          storages =
             fmap
                (StorageQuant.graphFromList (Map.keys timeFlowStates) .
                 StorageQuant.allEdgesFromSums) $
             Env.storageSequences $ Map.toList $
             fmap FlowTopo.topology timeFlowStates,
          states = timeFlowStates
       }


addSums ::
   (a -> a -> a) ->
   Sums a -> Sums a -> Sums a
addSums add (Sums sin0 sout0) (Sums sin1 sout1) =
   Sums (addSum add sin0 sin1) (addSum add sout0 sout1)

addSum ::
   (a -> a -> a) ->
   Maybe a -> Maybe a -> Maybe a
addSum add (Just fs0) (Just fs1) = Just (add fs0 fs1)
addSum _ Nothing Nothing = Nothing
addSum _ _ _ = error "addSum: inconsistent Maybes"


cumulateSequence ::
   (a -> a -> a) ->
   Map Idx.Section Idx.State ->
   Map Idx.Section a ->
   Map Idx.State a
cumulateSequence add secMap =
   Map.mapKeysWith add
      (MapU.checkedLookup "cumulateSequence" secMap)

cumulateSums ::
   (a -> a -> a) ->
   Map Idx.Section Idx.State ->
   PartMap Idx.Section a ->
   PartMap Idx.State a
cumulateSums add secMap =
   PartMap.mapKeysWith add
      (MapU.checkedLookup "cumulateSums" secMap)



mapCarryEdge ::
   (Ord sec, Show sec, Show state) =>
   String -> Map sec state ->
   StorageIdx.Edge sec -> StorageIdx.Edge state
mapCarryEdge caller secMap (StorageIdx.Edge from to) =
   StorageIdx.Edge
      (fmap (MapU.checkedLookup (caller ++ " from") secMap) from)
      (fmap (MapU.checkedLookup (caller ++ " to")   secMap) to)


withSection ::
   (idx node -> FlowTopo.Section node v -> Maybe r) ->
   Idx.InState idx node ->
   Graph node a v ->
   Maybe r
withSection f (Idx.InPart state idx) g =
   f idx =<< seqLookup state g

lookupSums ::
   (Ord node) =>
   Idx.StateNode node -> Graph node a v -> Maybe (Sums v)
lookupSums (Idx.PartNode state node) =
   Graph.lookupNode node . FlowTopo.topology <=< seqLookup state

seqLookup ::
   Idx.State -> Graph node a v -> Maybe (FlowTopo.Section node v)
seqLookup state = Map.lookup state . states


withStorage ::
   (Ord node) =>
   (idx -> StorageQuant.Graph Carry a -> Maybe a) ->
   Idx.ForStorage idx node -> Graph node a v -> Maybe a
withStorage look (Idx.ForStorage idx node) =
   look idx <=< Map.lookup node . storages


class
   (Env.Type (Env.TypeOf idx), Var.Index idx, Var.FormatIndex idx) =>
      Lookup idx where
   lookup ::
      (Ord node) =>
      idx node -> Graph node a v -> Maybe (Env.Element idx a v)

instance
   (LookupSignal idx, TopoVar.Index idx) =>
      Lookup (Idx.InState idx) where
   lookup = lookupSignal

instance
   (LookupScalar idx, StorageVar.Index idx) =>
      Lookup (Idx.ForStorage idx) where
   lookup = lookupScalar


class (TopoVar.Index idx) => LookupSignal idx where
   lookupSignal ::
      (Ord node) => Idx.InState idx node -> Graph node a v -> Maybe v

instance LookupSignal TopoIdx.Energy where
   lookupSignal = withSection FlowTopo.lookupEnergy

instance LookupSignal TopoIdx.Power where
   lookupSignal = withSection FlowTopo.lookupPower

instance LookupSignal TopoIdx.Eta where
   lookupSignal = withSection FlowTopo.lookupEta

instance LookupSignal TopoIdx.DTime where
   lookupSignal = withSection FlowTopo.lookupDTime

instance LookupSignal TopoIdx.X where
   lookupSignal = withSection FlowTopo.lookupX

instance LookupSignal TopoIdx.Sum where
   lookupSignal = withSection FlowTopo.lookupSum


class (StorageVar.Index idx) => LookupScalar idx where
   lookupScalar ::
      (Ord node) => Idx.ForStorage idx node -> Graph node a v -> Maybe a

instance LookupScalar (StorageIdx.Energy Idx.State) where
   lookupScalar = withStorage StorageQuant.lookupEnergy

instance LookupScalar (StorageIdx.X Idx.State) where
   lookupScalar = withStorage StorageQuant.lookupX

instance LookupScalar (StorageIdx.InSum Idx.State) where
   lookupScalar = withStorage StorageQuant.lookupInSum

instance LookupScalar (StorageIdx.OutSum Idx.State) where
   lookupScalar = withStorage StorageQuant.lookupOutSum


mapGraphWithVar ::
   (Ord node) =>
   (Var.ForStorageStateScalar node -> a0 -> a1) ->
   (Var.InStateSignal node -> v0 -> v1) ->
   Graph node a0 v0 ->
   Graph node a1 v1
mapGraphWithVar f g gr =
   StateFlow.Graph {
      storages = mapStoragesWithVar f gr,
      states   = mapStatesWithVar g $ states gr
   }

mapStoragesWithVar ::
   (Ord node) =>
   (Var.ForStorageStateScalar node -> a0 -> a1) ->
   Graph node a0 v0 ->
   Storages node a1
mapStoragesWithVar f gr =
   Map.mapWithKey (StorageQuant.mapGraphWithVar (flip lookupSums gr) f) $
   storages gr

mapStatesWithVar ::
   (Ord node) =>
   (Var.InStateSignal node -> v0 -> v1) ->
   States node v0 ->
   States node v1
mapStatesWithVar f =
   Map.mapWithKey $ \state -> FlowTopo.mapSectionWithVar (f . Idx.InPart state)
