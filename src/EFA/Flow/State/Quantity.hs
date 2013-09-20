{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module EFA.Flow.State.Quantity (
   Graph, StateFlow.states, StateFlow.storages,
   Topology, States, Storages,
   Sums(..), Sum(..), Carry(..), Flow(..),
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

   mapCarryWithVar,

   fromSequenceFlow,
   fromSequenceFlowResult,

   cumFromFlow,
   flowResultFromCum,
   flowResultFromCumResult,

   flowGraphFromCumResult,

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

import qualified EFA.Flow.Quantity as Quant
import qualified EFA.Flow.Sequence.Quantity as SeqFlowQuant
import qualified EFA.Flow.State.Index as StateIdx
import qualified EFA.Flow.State as StateFlow
import EFA.Flow.State (states, storages)
import EFA.Flow.Quantity
          (Sums(..), Sum(..), Flow(..), mapSums, traverseSums, (<#>))

import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Variable as Var

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph as Graph

import EFA.Equation.Arithmetic ((~+))
import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified EFA.Signal.Sequence as Sequ

import qualified EFA.Report.FormatValue as FormatValue

import qualified EFA.Utility.Map as MapU

import qualified Control.Monad.Trans.State as MS

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Stream as Stream; import Data.Stream (Stream)

import Control.Applicative (Applicative, pure, liftA2, (<*>), (<$>))
import Control.Monad (mplus, (<=<))
import Data.Traversable (Traversable, traverse, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Tuple.HT (mapSnd)
import Data.Maybe (isJust, fromMaybe)

import Prelude hiding (lookup, init, seq, sequence, sin, sum)



type
   Storages node a = StateFlow.Storages node a a (Carry a)

type
   States node a v =
      StateFlow.States node Graph.EitherEdge v (Sums a v) (Maybe (Flow v))

type
   Graph node a v =
      StateFlow.Graph node Graph.EitherEdge
         v (Sums a v) a a (Maybe (Flow v)) (Carry a)

data Carry a =
   Carry {
      carryEnergy, carryXOut, carryXIn :: a
   }


instance Quant.Carry Carry where
   carryEnergy = carryEnergy
   carryXOut   = carryXOut
   carryXIn    = carryXIn


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
      states   = mapStates   f g $ states gr,
      storages = mapStorages f   $ storages gr
   }

mapStates ::
   (a0 -> a1) ->
   (v0 -> v1) ->
   States node a0 v0 -> States node a1 v1
mapStates f g =
   fmap
      (\(dt, gr) ->
         (g dt,
          Graph.mapNode (mapSums f g) $
          Graph.mapEdge (fmap $ fmap g) gr))

mapStorages ::
   (a0 -> a1) ->
   Storages node a0 -> Storages node a1
mapStorages f =
   fmap
      (\((init, exit), edges) ->
         ((f init, f exit),
          fmap (fmap f) edges))


traverseGraph ::
   (Applicative f, Ord node) =>
   (a0 -> f a1) ->
   (v0 -> f v1) ->
   Graph node a0 v0 -> f (Graph node a1 v1)
traverseGraph f g (StateFlow.Graph sts seq) =
   liftA2 StateFlow.Graph
      (traverseStorages f   $ sts)
      (traverseStates   f g $ seq)

traverseStates ::
   (Applicative f, Ord node) =>
   (a0 -> f a1) ->
   (v0 -> f v1) ->
   States node a0 v0 -> f (States node a1 v1)
traverseStates f g =
   traverse
      (\(dt, gr) ->
         liftA2 (,) (g dt)
            (Graph.traverse (traverseSums f g) (traverse $ traverse g) gr))

traverseStorages ::
   (Applicative f, Ord node) =>
   (a0 -> f a1) ->
   Storages node a0 -> f (Storages node a1)
traverseStorages f =
   traverse
      (\((init, exit), edges) ->
         liftA2 (,)
            (liftA2 (,) (f init) (f exit))
            (traverse (traverse f) edges))



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
      StateFlow.Graph node Graph.EitherEdge a (Sums a a) a a (Maybe (Cum a)) a


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
          fmap (Graph.mapEdge (const ()) . Graph.mapNode (const ()) . snd . snd) sq
       sts =
          flip cumulateSequence secMap
             (\(dtime0, gr0) (dtime1, gr1) ->
                (add dtime0 dtime1,
                 Graph.checkedZipWith "StateFlow.fromSequenceFlow"
                    (addSums add)
                    (liftA2 (liftA2 add))
                    gr0 gr1)) $
          fmap ((mapSnd $ Graph.mapEdge $ fmap cumFromFlow) . snd) $
          SeqFlowQuant.mapSequence id integrate sq
   in  StateFlow.Graph {
          storages =
             Map.mapWithKey
                (\node (initExit, _, edges) ->
                   (initExit,
                    Map.union
                       (if allStEdges
                          then Map.fromList $
                               map (flip (,) zero) $
                               allStorageEdges (sumsMap node sts)
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
   StateFlow.States node Graph.EitherEdge v (Sums a v) (Maybe (Cum v)) ->
   Map Idx.State (Sums a v)
sumsMap node =
   fmap (fromMaybe (error "node not in sequence") .
         Graph.lookupNode node . snd)

allStorageEdges ::
   Map Idx.State (Sums a a) -> [Idx.StorageEdge Idx.State node]
allStorageEdges stores =
   case Map.partition (isJust . sumIn) stores of
      (ins, outs) ->
         liftA2 Idx.StorageEdge
            (Idx.Init : map Idx.NoInit (Map.keys ins))
            (Idx.Exit : map Idx.NoExit (Map.keys outs))


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
         fmap (mapSnd (fmap carryResultFromResult)) $
         StateFlow.storages gr,
      StateFlow.states =
         fmap (mapSnd (Graph.mapEdge (fmap flowResultFromCumResult))) $
         StateFlow.states gr
   }


addSums ::
   (a -> a -> a) ->
   Sums a a -> Sums a a -> Sums a a
addSums add (Sums sin0 sout0) (Sums sin1 sout1) =
   Sums (addSum add sin0 sin1) (addSum add sout0 sout1)

addSum ::
   (a -> a -> a) ->
   Maybe (Sum a a) -> Maybe (Sum a a) -> Maybe (Sum a a)
addSum add (Just (Sum cs0 fs0)) (Just (Sum cs1 fs1)) =
   Just (Sum (add cs0 cs1) (add fs0 fs1))
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
               (Quant.lookupEdge fieldOut se topo)
               (Quant.lookupEdge fieldIn (Idx.flip se) topo)


lookupEta :: (Ord node) => StateIdx.Eta node -> Graph node a v -> Maybe v
lookupEta =
   withTopology $ \(Idx.Eta se) -> Quant.lookupEdge flowEta se


lookupSum :: (Ord node) => StateIdx.Sum node -> Graph node a v -> Maybe v
lookupSum =
   withTopology $ \(Idx.Sum dir node) topo -> do
      sums <- Graph.lookupNode node topo
      fmap flowSum $
         case dir of
            Idx.In  -> sumIn sums
            Idx.Out -> sumOut sums


type FlowTopology node a v =
        Graph.Graph node Graph.EitherEdge (Sums a v) (Maybe (Flow v))

withTopology ::
   (idx node -> FlowTopology node a v -> Maybe r) ->
   Idx.InState idx node ->
   Graph node a v ->
   Maybe r
withTopology f (Idx.InPart state idx) g =
   f idx . snd =<< seqLookup state g


lookupDTime :: StateIdx.DTime node -> Graph node a v -> Maybe v
lookupDTime (Idx.InPart state Idx.DTime) =
   fmap fst . seqLookup state


lookupStEnergy ::
   (Ord node) => StateIdx.StEnergy node -> Graph node a v -> Maybe a
lookupStEnergy (Idx.ForNode (Idx.StEnergy se) node) g = do
   (_,edges) <- Map.lookup node $ storages g
   fmap carryEnergy $ Map.lookup se edges

lookupStX ::
   (Ord node) => StateIdx.StX node -> Graph node a v -> Maybe a
lookupStX (Idx.ForNode (Idx.StX se) node) g = do
   (_,edges) <- Map.lookup node $ storages g
   Idx.withStorageEdgeFromTrans
      (fmap carryXIn  . flip Map.lookup edges)
      (fmap carryXOut . flip Map.lookup edges)
      se

lookupStInSum ::
   (Ord node) => StateIdx.StInSum node -> Graph node a v -> Maybe a
lookupStInSum (Idx.ForNode (Idx.StInSum aug) node) g =
   case aug of
      Idx.Exit -> do
         ((_,exit),_) <- Map.lookup node $ storages g
         return exit
      Idx.NoExit state ->
         fmap carrySum . sumOut =<< lookupSums (Idx.stateNode state node) g

lookupStOutSum ::
   (Ord node) => StateIdx.StOutSum node -> Graph node a v -> Maybe a
lookupStOutSum (Idx.ForNode (Idx.StOutSum aug) node) g =
   case aug of
      Idx.Init -> do
         ((init,_),_) <- Map.lookup node $ storages g
         return init
      Idx.NoInit state ->
         fmap carrySum . sumIn =<< lookupSums (Idx.stateNode state node) g

lookupSums ::
   (Ord node) =>
   Idx.StateNode node -> Graph node a v -> Maybe (Sums a v)
lookupSums (Idx.PartNode state node) =
   Graph.lookupNode node . snd <=< seqLookup state

seqLookup ::
   Idx.State -> Graph node a v -> Maybe (v, FlowTopology node a v)
seqLookup state = Map.lookup state . states


type Element idx a v = Env.PartElement (Environment idx) a v

class
   (Env.AccessPart (Environment idx), Var.Index idx, Var.FormatIndex idx) =>
      Lookup idx where
   type Environment idx :: * -> * -> *
   lookup ::
      (Ord node) =>
      idx node -> Graph node a v -> Maybe (Element idx a v)

instance
   (LookupSignal idx, FormatValue.FormatSignalIndex idx) =>
      Lookup (Idx.InState idx) where
   type Environment (Idx.InState idx) = Env.Signal
   lookup = lookupSignal

instance
   (LookupScalar idx, FormatValue.FormatScalarIndex idx) =>
      Lookup (Idx.ForNode idx) where
   type Environment (Idx.ForNode idx) = Env.Scalar
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
      storages = mapStoragesWithVar f $ storages gr,
      states   = mapStatesWithVar f g $ states gr
   }

mapStoragesWithVar ::
   (Ord node) =>
   (Var.ForNodeStateScalar node -> a0 -> a1) ->
   Storages node a0 ->
   Storages node a1
mapStoragesWithVar f =
   Map.mapWithKey $ \node ((init, exit), edges) ->
      ((f (Idx.StOutSum Idx.Init <#> node) init,
        f (Idx.StInSum  Idx.Exit <#> node) exit),
       Map.mapWithKey (mapCarryWithVar f node) edges)

mapCarryWithVar ::
   (Var.ForNodeScalar part node -> a0 -> a1) ->
   node -> Idx.StorageEdge part node -> Carry a0 -> Carry a1
mapCarryWithVar f node edge =
   liftA2 f (Idx.ForNode <$> (carryVars <*> pure edge) <*> pure node)

carryVars :: Carry (Idx.StorageEdge part node -> Var.Scalar part node)
carryVars =
   Carry {
      carryEnergy = Var.scalarIndex . Idx.StEnergy,
      carryXOut = Var.scalarIndex . Idx.StX . Idx.storageTransFromEdge,
      carryXIn = Var.scalarIndex . Idx.StX . Idx.flip . Idx.storageTransFromEdge
   }

mapStatesWithVar ::
   (Ord node) =>
   (Var.ForNodeStateScalar node -> a0 -> a1) ->
   (Var.InStateSignal node -> v0 -> v1) ->
   States node a0 v0 ->
   States node a1 v1
mapStatesWithVar f g =
   Map.mapWithKey $ Quant.mapFlowTopologyWithVar f g
