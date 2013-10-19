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

   traverseGraph,
   traverseStorages,
   traverseStates,

   mapGraphWithVar,
   mapStoragesWithVar,
   mapStatesWithVar,

   fromSequenceFlow,
   fromSequenceFlowResult,

   cumFromFlow,
   flowResultFromCum,
   flowResultFromCumResult,

   flowGraphFromCumResult,
   flowGraphFromPlain,

   lookupPower,
   lookupEnergy,
   lookupX,
   lookupEta,
   lookupDTime,
   lookupSum,

   lookupStEnergy,
   lookupStX,
   lookupStInSum,
   lookupStOutSum,
   lookupSums,

   Lookup, lookup,
   LookupScalar, lookupScalar,
   LookupSignal, lookupSignal,
   Environment, Element, Env.switchPart,
   ) where

import qualified EFA.Flow.Sequence.Quantity as SeqFlowQuant
import qualified EFA.Flow.State.Index as StateIdx
import qualified EFA.Flow.State as StateFlow
import qualified EFA.Flow.StorageGraph.Quantity as StorageQuant
import qualified EFA.Flow.StorageGraph as StorageGraph
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Flow.PartMap as PartMap
import EFA.Flow.StorageGraph (StorageGraph(StorageGraph))
import EFA.Flow.PartMap (PartMap)
import EFA.Flow.State (states, storages)
import EFA.Flow.Topology.Quantity (Sums(..), Flow(..))

import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Variable as Var
import EFA.Equation.Environment (Environment, Element)
import EFA.Equation.Unknown (Unknown(unknown))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import EFA.Equation.Arithmetic ((~+))
import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified EFA.Signal.Sequence as Sequ

import qualified EFA.Utility.Map as MapU

import qualified Control.Monad.Trans.State as MS

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Stream as Stream; import Data.Stream (Stream)

import Control.Applicative (Applicative, pure, liftA2, (<*>))
import Control.Monad (mplus, (<=<))
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
         carryEnergy = Var.scalarIndex . Idx.StEnergy,
         carryXOut = Var.scalarIndex . Idx.StX . Idx.storageTransFromEdge,
         carryXIn = Var.scalarIndex . Idx.StX . Idx.flip . Idx.storageTransFromEdge
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
   fmap (StorageGraph.mapNode f . StorageGraph.mapEdge (fmap f))


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
   traverse (StorageGraph.traverse f (traverse f))



type Topology node nodeLabel = Graph.Graph node Graph.EitherEdge nodeLabel ()

_states ::
   (Ord node, Ord nodeLabel) =>
   Sequ.List (Topology node nodeLabel) ->
   Map (Topology node nodeLabel) Idx.State
_states =
   Map.fromAscList .
   flip zip [Idx.State 0 ..] .
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

stateMaps ::
   (Ord node, Ord nodeLabel) =>
   Map Idx.Section (Topology node nodeLabel) ->
   Map Idx.Section Idx.State
stateMaps =
   flip MS.evalState (Map.empty, Stream.iterate succ $ Idx.State 0) .
   traverse identify


type
   CumGraph node a =
      StateFlow.Graph node Graph.EitherEdge a (Sums a) a (Maybe (Cum a)) a


fromSequenceFlowGen ::
   (Ord node) =>
   (v -> a) ->
   (a -> a -> a) ->
   a ->
   Bool ->
   SeqFlowQuant.Graph node a v ->
   CumGraph node a
fromSequenceFlowGen integrate add zero allStEdges gr =
   let sq = SeqFlowQuant.sequence gr
       secMap =
          stateMaps $
          fmap (Graph.mapEdge (const ()) .
                Graph.mapNode (const ()) .
                FlowTopo.topology . snd) sq
       sts =
          flip cumulateSequence secMap
             (FlowTopoPlain.checkedZipWith "StateFlow.fromSequenceFlow"
                 add (addSums add) (liftA2 (liftA2 add))) $
          fmap (FlowTopoPlain.mapEdge (fmap cumFromFlow) . snd) $
          SeqFlowQuant.mapSequence integrate sq
   in  StateFlow.Graph {
          storages =
             Map.mapWithKey
                (\node (StorageGraph initExit edges, _) ->
                   StorageGraph
                      (cumulateSums add secMap initExit)
                      (Map.union
                          (if allStEdges
                             then Map.fromList $
                                  map (flip (,) zero) $
                                  StorageQuant.allEdgesFromSums (sumsMap node sts)
                             else Map.empty) $
                       cumulateStorageEdges add secMap $
                       fmap SeqFlowQuant.carryEnergy edges)) $
             SeqFlowQuant.storages gr,
          states = sts
       }


cumulateStorageEdges ::
   (Ord node) =>
   (a -> a -> a) ->
   Map Idx.Section Idx.State ->
   Map (Idx.StorageEdge Idx.Section node) a ->
   Map (Idx.StorageEdge Idx.State node) a
cumulateStorageEdges add secMap =
   Map.mapKeysWith add
      (mapStorageEdge "cumulateStorageEdges from" secMap)


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
      (SeqFlowQuant.flowEnergyOut flow)
      (SeqFlowQuant.flowEnergyIn flow)

flowResultFromCum :: Cum v -> Flow (Result v)
flowResultFromCum =
   flowResultFromCumResult . fmap Determined

flowResultFromCumResult :: Cum (Result v) -> Flow (Result v)
flowResultFromCumResult cum =
   (pure Undetermined) {
      SeqFlowQuant.flowEnergyOut = cumEnergyOut cum,
      SeqFlowQuant.flowEnergyIn  = cumEnergyIn  cum
   }

carryResultFromResult :: Result a -> Carry (Result a)
carryResultFromResult e =
   (pure Undetermined) {
      carryEnergy = e
   }

flowGraphFromCumResult ::
   CumGraph node (Result a) -> Graph node (Result a) (Result a)
flowGraphFromCumResult gr =
   StateFlow.Graph {
      StateFlow.storages =
         fmap (StorageGraph.mapEdge carryResultFromResult) $
         StateFlow.storages gr,
      StateFlow.states =
         fmap (FlowTopoPlain.mapEdge (fmap flowResultFromCumResult)) $
         StateFlow.states gr
   }

flowGraphFromPlain ::
   (Node.C node, Unknown a, Unknown v) =>
   StateFlow.Graph node Graph.EitherEdge
      () (Node.Type (Maybe Topo.StoreDir)) () () () ->
   Graph node a v
flowGraphFromPlain gr =
   StateFlow.Graph {
      StateFlow.storages =
         fmap (StorageGraph.mapNode (const unknown) .
               StorageGraph.mapEdge (const $ pure unknown)) $
         StateFlow.storages gr,
      StateFlow.states =
         fmap FlowTopo.sectionFromPlain $
         StateFlow.states gr
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



mapStorageEdge ::
   (Ord sec, Show sec, Show state) =>
   String -> Map sec state ->
   Idx.StorageEdge sec node -> Idx.StorageEdge state node
mapStorageEdge caller secMap (Idx.StorageEdge from to) =
   Idx.StorageEdge
      (fmap (MapU.checkedLookup (caller ++ " from") secMap) from)
      (fmap (MapU.checkedLookup (caller ++ " to")   secMap) to)


lookupPower ::
   (Ord node) => StateIdx.Power node -> Graph node a v -> Maybe v
lookupPower =
   lookupStruct flowPowerOut flowPowerIn (\(Idx.Power se) -> se)

lookupEnergy ::
   (Ord node) => StateIdx.Energy node -> Graph node a v -> Maybe v
lookupEnergy =
   lookupStruct flowEnergyOut flowEnergyIn (\(Idx.Energy se) -> se)

lookupX ::
   (Ord node) => StateIdx.X node -> Graph node a v -> Maybe v
lookupX =
   lookupStruct flowXOut flowXIn (\(Idx.X se) -> se)

lookupStruct ::
   (Ord node) =>
   (Flow v -> v) ->
   (Flow v -> v) ->
   (idx node -> Idx.StructureEdge node) ->
   Idx.InState idx node -> Graph node a v -> Maybe v
lookupStruct fieldOut fieldIn unpackIdx =
   withTopology $ \idx topo ->
      case unpackIdx idx of
         se ->
            mplus
               (FlowTopoPlain.lookupEdge fieldOut se topo)
               (FlowTopoPlain.lookupEdge fieldIn (Idx.flip se) topo)


lookupEta :: (Ord node) => StateIdx.Eta node -> Graph node a v -> Maybe v
lookupEta =
   withTopology $ \(Idx.Eta se) -> FlowTopoPlain.lookupEdge flowEta se


lookupSum :: (Ord node) => StateIdx.Sum node -> Graph node a v -> Maybe v
lookupSum =
   withTopology $ \(Idx.Sum dir node) topo -> do
      sums <- Graph.lookupNode node topo
      case dir of
         Idx.In  -> sumIn sums
         Idx.Out -> sumOut sums


type FlowTopology node v =
        Graph.Graph node Graph.EitherEdge (Sums v) (Maybe (Flow v))

withTopology ::
   (idx node -> FlowTopology node v -> Maybe r) ->
   Idx.InState idx node ->
   Graph node a v ->
   Maybe r
withTopology f (Idx.InPart state idx) g =
   f idx . FlowTopo.topology =<< seqLookup state g


lookupDTime :: StateIdx.DTime node -> Graph node a v -> Maybe v
lookupDTime (Idx.InPart state Idx.DTime) =
   fmap FlowTopo.label . seqLookup state


lookupStEnergy ::
   (Ord node) => StateIdx.StEnergy node -> Graph node a v -> Maybe a
lookupStEnergy (Idx.ForNode (Idx.StEnergy se) node) g = do
   sgr <- Map.lookup node $ storages g
   fmap carryEnergy $ StorageGraph.lookupEdge se sgr

lookupStX ::
   (Ord node) => StateIdx.StX node -> Graph node a v -> Maybe a
lookupStX (Idx.ForNode (Idx.StX se) node) g = do
   sgr <- Map.lookup node $ storages g
   Idx.withStorageEdgeFromTrans
      (fmap carryXIn  . flip StorageGraph.lookupEdge sgr)
      (fmap carryXOut . flip StorageGraph.lookupEdge sgr)
      se

{- |
It is an unchecked error if you lookup StInSum where is only an StOutSum.
-}
lookupStInSum ::
   (Ord node) => StateIdx.StInSum node -> Graph node a v -> Maybe a
lookupStInSum (Idx.ForNode (Idx.StInSum aug) node) g = do
   (StorageGraph partMap _) <- Map.lookup node $ storages g
   case aug of
      Idx.Exit -> return $ PartMap.exit partMap
      Idx.NoExit sec -> Map.lookup sec $ PartMap.parts partMap

{- |
It is an unchecked error if you lookup StOutSum where is only an StInSum.
-}
lookupStOutSum ::
   (Ord node) => StateIdx.StOutSum node -> Graph node a v -> Maybe a
lookupStOutSum (Idx.ForNode (Idx.StOutSum aug) node) g = do
   (StorageGraph partMap _) <- Map.lookup node $ storages g
   case aug of
      Idx.Init -> return $ PartMap.init partMap
      Idx.NoInit sec -> Map.lookup sec $ PartMap.parts partMap

lookupSums ::
   (Ord node) =>
   Idx.StateNode node -> Graph node a v -> Maybe (Sums v)
lookupSums (Idx.PartNode state node) =
   Graph.lookupNode node . FlowTopo.topology <=< seqLookup state

seqLookup ::
   Idx.State -> Graph node a v -> Maybe (FlowTopo.Section node v)
seqLookup state = Map.lookup state . states


class
   (Env.AccessPart (Environment idx), Var.Index idx, Var.FormatIndex idx) =>
      Lookup idx where
   lookup ::
      (Ord node) =>
      idx node -> Graph node a v -> Maybe (Element idx a v)

instance
   (LookupSignal idx, Var.SignalIndex idx) =>
      Lookup (Idx.InState idx) where
   lookup = lookupSignal

instance
   (LookupScalar idx, Var.ScalarIndex idx) =>
      Lookup (Idx.ForNode idx) where
   lookup = lookupScalar


class (Var.SignalIndex idx) => LookupSignal idx where
   lookupSignal ::
      (Ord node) => Idx.InState idx node -> Graph node a v -> Maybe v

instance LookupSignal Idx.Energy where
   lookupSignal = lookupEnergy

instance LookupSignal Idx.Power where
   lookupSignal = lookupPower

instance LookupSignal Idx.Eta where
   lookupSignal = lookupEta

instance LookupSignal Idx.DTime where
   lookupSignal = lookupDTime

instance LookupSignal Idx.X where
   lookupSignal = lookupX

instance LookupSignal Idx.Sum where
   lookupSignal = lookupSum


class (Var.ScalarIndex idx) => LookupScalar idx where
   lookupScalar ::
      (Ord node) => Idx.ForNode idx node -> Graph node a v -> Maybe a

instance LookupScalar (Idx.StEnergy Idx.State) where
   lookupScalar = lookupStEnergy

instance LookupScalar (Idx.StX Idx.State) where
   lookupScalar = lookupStX

instance LookupScalar (Idx.StInSum Idx.State) where
   lookupScalar = lookupStInSum

instance LookupScalar (Idx.StOutSum Idx.State) where
   lookupScalar = lookupStOutSum



{- |
If allStEdges
  Then: Insert all possible storage edges.
  Else: Insert only the storage edges that have counterparts in the sequence flow graph.
-}
fromSequenceFlow ::
   (Ord node, Arith.Constant a, a ~ Arith.Scalar v, Arith.Integrate v) =>
   Bool ->
   SeqFlowQuant.Graph node a v ->
   CumGraph node a
fromSequenceFlow =
   fromSequenceFlowGen Arith.integrate (~+) Arith.zero

fromSequenceFlowResult ::
   (Ord node, Arith.Constant a, a ~ Arith.Scalar v, Arith.Integrate v) =>
   Bool ->
   SeqFlowQuant.Graph node (Result a) (Result v) ->
   CumGraph node (Result a)
fromSequenceFlowResult =
   fromSequenceFlowGen (fmap Arith.integrate) (liftA2 (~+)) (pure Arith.zero)



mapGraphWithVar ::
   (Ord node) =>
   (Var.ForNodeStateScalar node -> a0 -> a1) ->
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
   (Var.ForNodeStateScalar node -> a0 -> a1) ->
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
