{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module EFA.Flow.Sequence.Quantity (
   Graph, SeqFlow.sequence, SeqFlow.storages,
   Topology, Sequence, Storages,
   Sums(..), Sum(..), Carry(..), Flow(..),

   mapGraph,
   mapStorages,
   mapSequence,

   traverseGraph,
   traverseStorages,
   traverseSequence,

   envFromGraph,
   graphFromEnv,

   graphFromPlain,
   storagesFromPlain,
   sequenceFromPlain,

   mapGraphWithVar,
   mapStoragesWithVar,
   mapSequenceWithVar,

   dirFromFlowGraph,

   lookupPower,
   lookupEnergy,
   lookupX,
   lookupEta,
   lookupDTime,
   lookupSum,

   lookupStorage,
   lookupMaxEnergy,
   lookupStEnergy,
   lookupStX,
   lookupStInSum,
   lookupStOutSum,
   lookupSums,

   Lookup, lookup,
   LookupScalar, lookupScalar,
   LookupSignal, lookupSignal,
   ) where

import qualified EFA.Application.Index as XIdx

import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Variable as Var

import qualified EFA.Flow.Sequence as SeqFlow
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr

import EFA.Flow.Sequence (sequence, storages)

import Control.Monad (mplus, (<=<))
import Control.Applicative (Applicative, pure, liftA2, liftA3, (<*>), (<$))

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Foldable as Fold

import Data.Traversable (Traversable, traverse, foldMapDefault)
import Data.Foldable (Foldable)
import Data.Tuple.HT (mapSnd)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, (<>))

import Prelude hiding (lookup, init, seq, sequence, sin, sum)


type
   Storages node a = SeqFlow.Storages node a a a (Carry a)

type
   Topology node a v =
      Gr.Graph node Gr.DirEdge (Sums a v) (Flow v)

type
   Sequence node a v = SeqFlow.Sequence node Gr.DirEdge v (Sums a v) (Flow v)

type
   Graph node a v =
      SeqFlow.Graph node Gr.DirEdge
         v (Sums a v) a a a (Flow v) (Carry a)

data Carry a =
   Carry {
      carryMaxEnergy, carryEnergy, carryXOut, carryXIn :: a
   }

data Flow v =
   Flow {
      flowPowerOut, flowEnergyOut, flowXOut,
      flowEta,
      flowXIn, flowEnergyIn, flowPowerIn :: v
   }

data Sums a v =
   Sums { sumIn, sumOut :: Maybe (Sum a v) }

data Sum a v =
   Sum { carrySum :: a, flowSum :: v }


instance Functor Carry where
   fmap f (Carry me e xout xin) =
      Carry (f me) (f e) (f xout) (f xin)

instance Functor Flow where
   fmap f (Flow pout eout xout eta xin ein pin) =
      Flow (f pout) (f eout) (f xout) (f eta) (f xin) (f ein) (f pin)


instance Foldable Carry where
   foldMap = foldMapDefault

instance Foldable Flow where
   foldMap = foldMapDefault


instance Traversable Carry where
   traverse f (Carry me e xout xin) =
      pure Carry <*> f me <*> f e <*> f xout <*> f xin

instance Traversable Flow where
   traverse f (Flow pout eout xout eta xin ein pin) =
      pure Flow <*> f pout <*> f eout <*> f xout <*> f eta <*> f xin <*> f ein <*> f pin


instance Applicative Carry where
   pure a = Carry a a a a
   Carry fme fe fxout fxin <*> Carry me e xout xin =
      Carry (fme me) (fe e) (fxout xout) (fxin xin)

instance Applicative Flow where
   pure a = Flow a a a a a a a
   Flow fpout feout fxout feta fxin fein fpin
         <*> Flow pout eout xout eta xin ein pin =
      Flow
         (fpout pout) (feout eout) (fxout xout)
         (feta eta) (fxin xin) (fein ein) (fpin pin)


mapGraph ::
   (a0 -> a1) ->
   (v0 -> v1) ->
   Graph node a0 v0 -> Graph node a1 v1
mapGraph f g gr =
   SeqFlow.Graph {
      sequence = mapSequence f g $ sequence gr,
      storages = mapStorages f   $ storages gr
   }

mapSequence ::
   (a0 -> a1) ->
   (v0 -> v1) ->
   Sequence node a0 v0 -> Sequence node a1 v1
mapSequence f g =
   fmap
      (\(rng, (dt, gr)) ->
         (rng,
          (g dt,
           Gr.mapNode (mapSums f g) $
           Gr.mapEdge (fmap g) gr)))

mapSums ::
   (a0 -> a1) ->
   (v0 -> v1) ->
   Sums a0 v0 -> Sums a1 v1
mapSums f g s =
   Sums {
      sumIn  = fmap (mapSum f g) $ sumIn  s,
      sumOut = fmap (mapSum f g) $ sumOut s
   }

mapSum ::
   (a0 -> a1) ->
   (v0 -> v1) ->
   Sum a0 v0 -> Sum a1 v1
mapSum f g s =
   Sum {
      carrySum = f $ carrySum s,
      flowSum  = g $ flowSum  s
   }

mapStorages ::
   (a0 -> a1) ->
   Storages node a0 -> Storages node a1
mapStorages f =
   fmap
      (\((init, exit), storage, edges) ->
         ((f init, f exit),
          fmap f storage,
          fmap (fmap f) edges))


traverseGraph ::
   (Applicative f) =>
   (a0 -> f a1) ->
   (v0 -> f v1) ->
   Graph node a0 v0 -> f (Graph node a1 v1)
traverseGraph f g (SeqFlow.Graph sts seq) =
   liftA2 SeqFlow.Graph
      (traverseStorages f   $ sts)
      (traverseSequence f g $ seq)

traverseSequence ::
   (Applicative f) =>
   (a0 -> f a1) ->
   (v0 -> f v1) ->
   Sequence node a0 v0 -> f (Sequence node a1 v1)
traverseSequence f g =
   traverse
      (\(rng, (dt, gr)) ->
         fmap ((,) rng) $
         liftA2 (,) (g dt)
            (Gr.traverse (traverseSums f g) (traverse g) gr))

traverseSums ::
   (Applicative f) =>
   (a0 -> f a1) ->
   (v0 -> f v1) ->
   Sums a0 v0 -> f (Sums a1 v1)
traverseSums f g (Sums i o) =
   liftA2 Sums
      (traverse (traverseSum f g) i)
      (traverse (traverseSum f g) o)

traverseSum ::
   (Applicative f) =>
   (a0 -> f a1) ->
   (v0 -> f v1) ->
   Sum a0 v0 -> f (Sum a1 v1)
traverseSum f g (Sum cs fs) =
   liftA2 Sum (f cs) (g fs)

traverseStorages ::
   (Applicative f) =>
   (a0 -> f a1) ->
   Storages node a0 -> f (Storages node a1)
traverseStorages f =
   traverse
      (\((init, exit), storage, edges) ->
         liftA3 (,,)
            (liftA2 (,) (f init) (f exit))
            (traverse f storage)
            (traverse (traverse f) edges))


envFromGraph ::
   (Ord node) =>
   Graph node a v -> Env.Complete node a v
envFromGraph g =
   case envFromSequence $ sequence g of
      ((stInSumMap, stOutSumMap), envSignal) ->
         Env.Complete
            (mempty
                {Env.stInSumMap = stInSumMap,
                 Env.stOutSumMap = stOutSumMap} <>
             (envFromStorages $ storages g))
            envSignal

envFromStorages ::
   (Ord node) =>
   Storages node a -> Env.Scalar node a
envFromStorages =
   Fold.fold .
   Map.mapWithKey
      (\node ((init, exit), storage, edges) ->
         Env.Scalar {
            Env.maxEnergyMap =
               nodeMap node Idx.MaxEnergy $ fmap carryMaxEnergy edges,
            Env.stEnergyMap =
               nodeMap node Idx.StEnergy $ fmap carryEnergy edges,
            Env.stXMap =
               Map.unionWith
                  (error "envFromStorages: duplicate X indices")
                  (nodeMap node (Idx.StX . Idx.storageTransFromEdge) $
                   fmap carryXOut edges)
                  (nodeMap node (Idx.StX . Idx.flip . Idx.storageTransFromEdge) $
                   fmap carryXIn edges),
            Env.storageMap =
               nodeMap node Idx.Storage storage,
            Env.stOutSumMap =
               Map.singleton (XIdx.stOutSum XIdx.initSection node) init,
            Env.stInSumMap =
               Map.singleton (XIdx.stInSum  XIdx.exitSection node) exit
         })

nodeMap ::
   (Ord node, Ord (idx node)) =>
   node -> (k -> idx node) -> Map k a -> Map (Idx.ForNode idx node) a
nodeMap node f =
   Map.mapKeys (flip Idx.ForNode node . f)


envFromSequence ::
   (Ord node) =>
   Sequence node a v ->
   ((Env.StInSumMap node a, Env.StOutSumMap node a),
    Env.Signal node v)
envFromSequence =
   Fold.fold .
   Map.mapWithKey
      (\sec (_rng, (dtime, topo)) ->
         let nls = Gr.nodeLabels topo
             els = Gr.edgeLabels topo
             sumOutMap = Map.mapMaybe sumOut nls
             sumInMap  = Map.mapMaybe sumIn nls
         in  ((Map.mapKeys (Idx.ForNode $ Idx.StInSum $ Idx.NoExit sec) $
               fmap carrySum sumOutMap,
               Map.mapKeys (Idx.ForNode $ Idx.StOutSum $ Idx.NoInit sec) $
               fmap carrySum sumInMap),
              Env.Signal {
                 Env.powerMap =
                    Map.unionWith
                       (error "envFromSequence: duplicate power indices")
                       (edgeMap sec Idx.Power $ fmap flowPowerOut els)
                       (edgeMap sec (Idx.Power . Idx.flip) $ fmap flowPowerIn els),
                 Env.energyMap =
                    Map.unionWith
                       (error "envFromSequence: duplicate energy indices")
                       (edgeMap sec Idx.Energy $ fmap flowEnergyOut els)
                       (edgeMap sec (Idx.Energy . Idx.flip) $ fmap flowEnergyIn els),
                 Env.xMap =
                    Map.unionWith
                       (error "envFromSequence: duplicate X indices")
                       (edgeMap sec Idx.X $ fmap flowXOut els)
                       (edgeMap sec (Idx.X . Idx.flip) $ fmap flowXIn els),
                 Env.etaMap = edgeMap sec Idx.Eta $ fmap flowEta els,
                 Env.sumMap =
                    Map.unionWith
                       (error "envFromSequence: duplicate Sum indices")
                       (Map.mapKeys (Idx.InPart sec . Idx.Sum Idx.In) $
                        fmap flowSum sumInMap)
                       (Map.mapKeys (Idx.InPart sec . Idx.Sum Idx.Out) $
                        fmap flowSum sumOutMap),
                 Env.dtimeMap = Map.singleton (XIdx.dTime sec) dtime
              }))

edgeMap ::
   (Ord part, Ord (idx node)) =>
   part ->
   (Idx.StructureEdge node -> idx node) ->
   Map (Gr.DirEdge node) a ->
   Map (Idx.InPart part idx node) a
edgeMap sec f =
   Map.mapKeys
      (Idx.InPart sec . f . Topo.structureEdgeFromDirEdge)


graphFromEnv ::
   (Ord node) =>
   Env.Complete node a v ->
   SeqFlow.RangeGraph node -> Graph node a v
graphFromEnv (Env.Complete envScalar envSignal) =
   mapGraphWithVar
      (\idx () ->
         fromMaybe (error "graphFromEnv.lookupScalar") $
         Env.lookupScalar idx envScalar)
      (\idx () ->
         fromMaybe (error "graphFromEnv.lookupSignal") $
         Env.lookupSignal idx envSignal) .
   graphFromPlain


lookupPower ::
   (Ord node) => XIdx.Power node -> Graph node a v -> Maybe v
lookupPower =
   lookupStruct flowPowerOut flowPowerIn (\(Idx.Power se) -> se)

lookupEnergy ::
   (Ord node) => XIdx.Energy node -> Graph node a v -> Maybe v
lookupEnergy =
   lookupStruct flowEnergyOut flowEnergyIn (\(Idx.Energy se) -> se)

lookupX ::
   (Ord node) => XIdx.X node -> Graph node a v -> Maybe v
lookupX =
   lookupStruct flowXOut flowXIn (\(Idx.X se) -> se)

lookupStruct ::
   (Ord node) =>
   (Flow v -> v) ->
   (Flow v -> v) ->
   (idx node -> Idx.StructureEdge node) ->
   Idx.InSection idx node -> Graph node a v -> Maybe v
lookupStruct fieldOut fieldIn unpackIdx =
   withTopology $ \idx topo ->
      case unpackIdx idx of
         se ->
            mplus
               (fmap fieldOut $
                Gr.lookupEdge (Topo.dirEdgeFromStructureEdge se) topo)
               (fmap fieldIn $
                Gr.lookupEdge (Topo.dirEdgeFromStructureEdge $ Idx.flip se) topo)


lookupEta :: (Ord node) => XIdx.Eta node -> Graph node a v -> Maybe v
lookupEta =
   withTopology $ \(Idx.Eta se) topo ->
      fmap flowEta $ Gr.lookupEdge (Topo.dirEdgeFromStructureEdge se) topo


lookupSum :: (Ord node) => XIdx.Sum node -> Graph node a v -> Maybe v
lookupSum =
   withTopology $ \(Idx.Sum dir node) topo -> do
      sums <- Gr.lookupNode node topo
      fmap flowSum $
         case dir of
            Idx.In  -> sumIn sums
            Idx.Out -> sumOut sums


withTopology ::
   (idx node -> Topology node a v -> Maybe r) ->
   Idx.InSection idx node ->
   Graph node a v ->
   Maybe r
withTopology f (Idx.InPart sec idx) g =
   f idx . snd =<< seqLookup sec g


lookupDTime :: XIdx.DTime node -> Graph node a v -> Maybe v
lookupDTime (Idx.InPart sec Idx.DTime) =
   fmap fst . seqLookup sec


lookupStorage ::
   (Ord node) => XIdx.Storage node -> Graph node a v -> Maybe a
lookupStorage (Idx.ForNode (Idx.Storage bnd) node) g = do
   (_,stores,_) <- Map.lookup node $ storages g
   Map.lookup bnd stores

lookupMaxEnergy ::
   (Ord node) => XIdx.MaxEnergy node -> Graph node a v -> Maybe a
lookupMaxEnergy (Idx.ForNode (Idx.MaxEnergy se) node) g = do
   (_,_,edges) <- Map.lookup node $ storages g
   fmap carryMaxEnergy $ Map.lookup se edges

lookupStEnergy ::
   (Ord node) => XIdx.StEnergy node -> Graph node a v -> Maybe a
lookupStEnergy (Idx.ForNode (Idx.StEnergy se) node) g = do
   (_,_,edges) <- Map.lookup node $ storages g
   fmap carryEnergy $ Map.lookup se edges

lookupStX ::
   (Ord node) => XIdx.StX node -> Graph node a v -> Maybe a
lookupStX (Idx.ForNode (Idx.StX se) node) g = do
   (_,_,edges) <- Map.lookup node $ storages g
   Idx.withStorageEdgeFromTrans
      (fmap carryXIn  . flip Map.lookup edges)
      (fmap carryXOut . flip Map.lookup edges)
      se

lookupStInSum ::
   (Ord node) => XIdx.StInSum node -> Graph node a v -> Maybe a
lookupStInSum (Idx.ForNode (Idx.StInSum aug) node) g =
   case aug of
      Idx.Exit -> do
         ((_,exit),_,_) <- Map.lookup node $ storages g
         return exit
      Idx.NoExit sec ->
         fmap carrySum . sumOut =<< lookupSums (Idx.secNode sec node) g

lookupStOutSum ::
   (Ord node) => XIdx.StOutSum node -> Graph node a v -> Maybe a
lookupStOutSum (Idx.ForNode (Idx.StOutSum aug) node) g =
   case aug of
      Idx.Init -> do
         ((init,_),_,_) <- Map.lookup node $ storages g
         return init
      Idx.NoInit sec ->
         fmap carrySum . sumIn =<< lookupSums (Idx.secNode sec node) g

lookupSums ::
   (Ord node) =>
   Idx.SecNode node -> Graph node a v -> Maybe (Sums a v)
lookupSums (Idx.PartNode sec node) =
   Gr.lookupNode node . snd <=< seqLookup sec

seqLookup ::
   Idx.Section -> Graph node a v -> Maybe (v, Topology node a v)
seqLookup sec = fmap snd . Map.lookup sec . sequence


class (Env.AccessPart (Env.Environment idx), Var.Index idx) => Lookup idx where
   lookup ::
      (Ord node) =>
      idx node -> Graph node a v -> Maybe (Env.Element idx a v)

instance (LookupSignal idx) => Lookup (Idx.InSection idx) where
   lookup = lookupSignal

instance (LookupScalar idx) => Lookup (Idx.ForNode idx) where
   lookup = lookupScalar


class (Var.SignalIndex idx) => LookupSignal idx where
   lookupSignal ::
      (Ord node) => Idx.InSection idx node -> Graph node a v -> Maybe v

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

instance LookupScalar Idx.MaxEnergy where
   lookupScalar = lookupMaxEnergy

instance LookupScalar Idx.Storage where
   lookupScalar = lookupStorage

instance LookupScalar (Idx.StEnergy Idx.Section) where
   lookupScalar = lookupStEnergy

instance LookupScalar (Idx.StX Idx.Section) where
   lookupScalar = lookupStX

instance LookupScalar (Idx.StInSum Idx.Section) where
   lookupScalar = lookupStInSum

instance LookupScalar (Idx.StOutSum Idx.Section) where
   lookupScalar = lookupStOutSum



graphFromPlain ::
   (Ord node) =>
   SeqFlow.RangeGraph node -> Graph node () ()
graphFromPlain g =
   SeqFlow.Graph {
      storages = storagesFromPlain $ SeqFlow.storages g,
      sequence =
         sequenceFromPlain $
         fmap (mapSnd (mapSnd dirFromFlowGraph)) $ SeqFlow.sequence g
   }

dirFromFlowGraph ::
   (Ord n) =>
   Gr.Graph n Gr.EitherEdge nl el -> Gr.Graph n Gr.DirEdge nl el
dirFromFlowGraph =
   Gr.mapEdgesMaybe $ \ee ->
      case ee of
         Gr.EDirEdge de -> Just de
         Gr.EUnDirEdge _ -> Nothing


storagesFromPlain ::
   (Ord node) =>
   SeqFlow.Storages node SeqFlow.InitIn SeqFlow.ExitOut () () ->
   Storages node ()
storagesFromPlain =
   Map.map $
      \(_initExit, bnds, edges) ->
         (((), ()),
          () <$ bnds,
          pure () <$ edges)


sequenceFromPlain ::
   (Ord node) =>
   SeqFlow.Sequence node Gr.DirEdge ()
      (Topo.NodeType (Maybe Topo.StoreDir)) () ->
   Sequence node () ()
sequenceFromPlain =
   let sum = Sum () ()
       inSum   = Sums { sumIn = Nothing,  sumOut = Nothing }
       outSum  = Sums { sumIn = Just sum, sumOut = Nothing }
       noSum   = Sums { sumIn = Nothing,  sumOut = Just sum }
       bothSum = Sums { sumIn = Just sum, sumOut = Just sum }
   in  Map.map $ \(rng, ((), gr)) ->
          (,) rng $
          ((),
           Gr.mapNode
              (\nt ->
                 case nt of
                    Topo.Storage Nothing -> noSum
                    Topo.Storage (Just Topo.In) -> inSum
                    Topo.Storage (Just Topo.Out) -> outSum
                    Topo.Sink -> inSum
                    Topo.AlwaysSink -> inSum
                    Topo.Source -> outSum
                    Topo.AlwaysSource -> outSum
                    Topo.Crossing -> bothSum
                    Topo.DeadNode -> noSum
                    Topo.NoRestriction -> bothSum) $
           Gr.mapEdge (const $ pure ()) gr)



mapGraphWithVar ::
   (Ord node) =>
   (Var.ForNodeSectionScalar node -> a0 -> a1) ->
   (Var.InSectionSignal node -> v0 -> v1) ->
   Graph node a0 v0 ->
   Graph node a1 v1
mapGraphWithVar f g gr =
   SeqFlow.Graph {
      storages = mapStoragesWithVar f $ storages gr,
      sequence = mapSequenceWithVar f g $ sequence gr
   }

mapStoragesWithVar ::
   (Ord node) =>
   (Var.ForNodeSectionScalar node -> a0 -> a1) ->
   Storages node a0 ->
   Storages node a1
mapStoragesWithVar f =
   let applyScalar idx node = f $ Idx.ForNode (Var.scalarIndex idx) node
   in  Map.mapWithKey $ \node ((init, exit), bnds, edges) ->
          ((applyScalar (Idx.StOutSum Idx.initSection) node init,
            applyScalar (Idx.StInSum  Idx.exitSection) node exit),
           Map.mapWithKey
              (\bnd a -> applyScalar (Idx.Storage bnd) node a)
              bnds,
           Map.mapWithKey
              (\edge carry ->
                 Carry {
                    carryMaxEnergy = applyScalar (Idx.MaxEnergy edge),
                    carryEnergy = applyScalar (Idx.StEnergy edge),
                    carryXOut = applyScalar (Idx.StX $ Idx.storageTransFromEdge edge),
                    carryXIn = applyScalar (Idx.StX $ Idx.flip $ Idx.storageTransFromEdge edge)
                 } <*> pure node <*> carry)
              edges)

mapSequenceWithVar ::
   (Ord node) =>
   (Var.ForNodeSectionScalar node -> a0 -> a1) ->
   (Var.InSectionSignal node -> v0 -> v1) ->
   Sequence node a0 v0 ->
   Sequence node a1 v1
mapSequenceWithVar f g =
   let applyScalar idx = f $ Idx.liftForNode Var.scalarIndex idx
       applySignal idx = g $ Idx.liftInPart Var.signalIndex idx
       applyFlow idx sec e =
          g $ Idx.InPart sec $ Var.signalIndex $ idx $
          Topo.structureEdgeFromDirEdge e

   in  Map.mapWithKey $ \sec (rng, (dtime, gr)) ->
          (,) rng $
          (applySignal (XIdx.dTime sec) dtime,
           Gr.mapNodeWithKey
              (\n (Sums {sumIn = sin, sumOut = sout}) ->
                 Sums {
                    sumIn =
                       flip fmap sin $ \(Sum {carrySum = cs, flowSum = fs}) ->
                          Sum
                             (applyScalar (XIdx.stOutSum sec n) cs)
                             (applySignal (XIdx.inSum sec n) fs),
                    sumOut =
                       flip fmap sout $ \(Sum {carrySum = cs, flowSum = fs}) ->
                          Sum
                             (applyScalar (XIdx.stInSum sec n) cs)
                             (applySignal (XIdx.outSum sec n) fs)
                 }) $
           Gr.mapEdgeWithKey
              (\e flow ->
                 Flow {
                    flowPowerOut = applyFlow Idx.Power,
                    flowPowerIn = applyFlow (Idx.Power . Idx.flip),
                    flowEnergyOut = applyFlow Idx.Energy,
                    flowEnergyIn = applyFlow (Idx.Energy . Idx.flip),
                    flowXOut = applyFlow Idx.X,
                    flowXIn = applyFlow (Idx.X . Idx.flip),
                    flowEta = applyFlow Idx.Eta
                 } <*> pure sec <*> pure e <*> flow)
              gr)
