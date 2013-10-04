{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module EFA.Flow.Sequence.Quantity (
   Graph, SeqFlow.sequence, SeqFlow.storages,
   Topology, Sequence, Storages,
   Sums(..), Sum(..), Carry(..), Flow(..),

   mapGraph,
   mapStorages,
   mapSequence,

   checkedZipWithGraph,
   checkedZipWithSequence,
   checkedZipWithStorages,

   traverseGraph,
   traverseStorages,
   traverseSequence,

   foldMap,
   fold,

   envFromGraph,
   graphFromEnv,
   toAssignMap,

   Unknown(..),
   graphFromPlain,
   storagesFromPlain,
   sequenceFromPlain,
   unknownTopologyNodes,

   mapGraphWithVar,
   mapStoragesWithVar,
   mapSequenceWithVar,

   mapCarryWithVar,

   Quant.liftEdgeFlow,
   Quant.dirFromFlowGraph,

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

   lookupEnergyTopology,

   Lookup, lookup,
   LookupScalar, lookupScalar,
   LookupSignal, lookupSignal,
   Env.Environment, Env.Element, Env.switchPart,

   formatAssigns,
   ) where

import qualified EFA.Flow.Sequence.AssignMap as AssignMap
import qualified EFA.Flow.Sequence.Index as SeqIdx
import qualified EFA.Flow.Sequence as SeqFlow
import qualified EFA.Flow.Quantity as Quant
import EFA.Flow.Sequence.AssignMap (AssignMap)
import EFA.Flow.Sequence (sequence, storages)
import EFA.Flow.Quantity
          (Topology, Sums(..), Sum(..), Flow(..),
           mapSums, zipWithSums, traverseSums, (<#>))

import qualified EFA.Signal.Sequence as Sequ

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Variable as Var
import EFA.Equation.Result (Result(Undetermined))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import qualified EFA.Report.FormatValue as FormatValue
import EFA.Report.FormatValue (FormatValue, formatAssign)
import EFA.Report.Format (Format)

import qualified EFA.Utility.Map as MapU
import EFA.Utility.Map (Caller)

import qualified Control.Monad.Trans.Writer as MW
import Control.Monad (mplus, (<=<))
import Control.Applicative (Applicative, pure, liftA2, liftA3, (<*>), (<$>), (<$))

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Foldable as Fold

import Data.Traversable (Traversable, traverse, foldMapDefault)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid, mempty, (<>))
import Data.Maybe.HT (toMaybe)

import Prelude hiding (lookup, init, seq, sequence, sin, sum)


type
   Storages node a = SeqFlow.Storages node a a a (Carry a)

type
   Sequence node a v =
      SeqFlow.Sequence node Graph.EitherEdge v (Sums a v) (Maybe (Flow v))

type
   Graph node a v =
      SeqFlow.Graph node Graph.EitherEdge
         v (Sums a v) a a a (Maybe (Flow v)) (Carry a)

data Carry a =
   Carry {
      carryMaxEnergy, carryEnergy, carryXOut, carryXIn :: a
   }
   deriving (Eq)


instance Quant.Carry Carry where
   carryEnergy = carryEnergy
   carryXOut   = carryXOut
   carryXIn    = carryXIn


instance Functor Carry where
   fmap f (Carry me e xout xin) =
      Carry (f me) (f e) (f xout) (f xin)


instance Foldable Carry where
   foldMap = foldMapDefault


instance Traversable Carry where
   traverse f (Carry me e xout xin) =
      pure Carry <*> f me <*> f e <*> f xout <*> f xin


instance Applicative Carry where
   pure a = Carry a a a a
   Carry fme fe fxout fxin <*> Carry me e xout xin =
      Carry (fme me) (fe e) (fxout xout) (fxin xin)


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
           Graph.mapNode (mapSums f g) $
           Graph.mapEdge (fmap $ fmap g) gr)))

mapStorages ::
   (a0 -> a1) ->
   Storages node a0 -> Storages node a1
mapStorages f =
   fmap
      (\((init, exit), storage, edges) ->
         ((f init, f exit),
          fmap f storage,
          fmap (fmap f) edges))


checkedZipWithGraph ::
   (Ord node) =>
   Caller ->
   (a0 -> a1 -> a2) ->
   (v0 -> v1 -> v2) ->
   Graph node a0 v0 ->
   Graph node a1 v1 ->
   Graph node a2 v2
checkedZipWithGraph caller f g gr0 gr1 =
   SeqFlow.Graph {
      sequence = checkedZipWithSequence caller f g (sequence gr0) (sequence gr1),
      storages = checkedZipWithStorages caller f   (storages gr0) (storages gr1)
   }

checkedZipWithSequence ::
   (Ord node) =>
   Caller ->
   (a0 -> a1 -> a2) ->
   (v0 -> v1 -> v2) ->
   Sequence node a0 v0 ->
   Sequence node a1 v1 ->
   Sequence node a2 v2
checkedZipWithSequence caller f g =
   MapU.checkedZipWith (caller++".checkedZipWithSequence")
      (\(rng0, (dt0, gr0)) (rng1, (dt1, gr1)) ->
         (if rng0==rng1 then rng0 else error (caller++".equalRange"),
          (g dt0 dt1,
           Graph.checkedZipWith
              (caller++".checkedZipWithSequence.section")
              (zipWithSums f g)
              (liftA2 $ liftA2 g)
              gr0 gr1)))

checkedZipWithStorages ::
   (Ord node) =>
   Caller ->
   (a0 -> a1 -> a2) ->
   Storages node a0 ->
   Storages node a1 ->
   Storages node a2
checkedZipWithStorages caller f =
   MapU.checkedZipWith (caller++".checkedZipWithStorages")
      (\((init0, exit0), storage0, edges0)
        ((init1, exit1), storage1, edges1) ->
         ((f init0 init1, f exit0 exit1),
          MapU.checkedZipWith (caller++".checkedZipWithStorages.storage")
             f storage0 storage1,
          MapU.checkedZipWith(caller++".checkedZipWithStorages.edges")
             (liftA2 f) edges0 edges1))


traverseGraph ::
   (Applicative f, Ord node) =>
   (a0 -> f a1) ->
   (v0 -> f v1) ->
   Graph node a0 v0 -> f (Graph node a1 v1)
traverseGraph f g (SeqFlow.Graph sts seq) =
   liftA2 SeqFlow.Graph
      (traverseStorages f   $ sts)
      (traverseSequence f g $ seq)

traverseSequence ::
   (Applicative f, Ord node) =>
   (a0 -> f a1) ->
   (v0 -> f v1) ->
   Sequence node a0 v0 -> f (Sequence node a1 v1)
traverseSequence f g =
   traverse
      (\(rng, (dt, gr)) ->
         fmap ((,) rng) $
         liftA2 (,) (g dt)
            (Graph.traverse (traverseSums f g) (traverse $ traverse g) gr))

traverseStorages ::
   (Applicative f, Ord node) =>
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
               Map.singleton (SeqIdx.stOutSum SeqIdx.initSection node) init,
            Env.stInSumMap =
               Map.singleton (SeqIdx.stInSum  SeqIdx.exitSection node) exit
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
         let nls = Graph.nodeLabels topo
             els = Graph.edgeLabels $ Quant.dirFromFlowGraph topo
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
                 Env.dtimeMap = Map.singleton (SeqIdx.dTime sec) dtime
              }))

edgeMap ::
   (Ord part, Ord (idx node)) =>
   part ->
   (Idx.StructureEdge node -> idx node) ->
   Map (Graph.DirEdge node) a ->
   Map (Idx.InPart part idx node) a
edgeMap sec f =
   Map.mapKeys
      (Idx.InPart sec . f . Topo.structureEdgeFromDirEdge)


graphFromEnv ::
   (Node.C node) =>
   Env.Complete node a v ->
   SeqFlow.RangeGraph node -> Graph node a v
graphFromEnv (Env.Complete envScalar envSignal) =
   mapGraphWithVar
      (\idx Irrelevant ->
         Var.checkedLookup "graphFromEnv.lookupScalar"
            Env.lookupScalar idx envScalar)
      (\idx Irrelevant ->
         Var.checkedLookup "graphFromEnv.lookupSignal"
            Env.lookupSignal idx envSignal) .
   graphFromPlain


toAssignMap ::
   (Node.C node) =>
   Graph node a v -> AssignMap node a v
toAssignMap =
   fold .
   mapGraphWithVar AssignMap.scalarSingleton AssignMap.signalSingleton


lookupPower ::
   (Ord node) => SeqIdx.Power node -> Graph node a v -> Maybe v
lookupPower =
   lookupStruct flowPowerOut flowPowerIn (\(Idx.Power se) -> se)

lookupEnergy ::
   (Ord node) => SeqIdx.Energy node -> Graph node a v -> Maybe v
lookupEnergy =
   lookupStruct flowEnergyOut flowEnergyIn (\(Idx.Energy se) -> se)

lookupEnergyTopology ::
   (Ord node) => Idx.Energy node -> Topology node a v -> Maybe v
lookupEnergyTopology =
   lookupStructTopology flowEnergyOut flowEnergyIn (\(Idx.Energy se) -> se)

lookupX ::
   (Ord node) => SeqIdx.X node -> Graph node a v -> Maybe v
lookupX =
   lookupStruct flowXOut flowXIn (\(Idx.X se) -> se)

lookupStruct ::
   (Ord node) =>
   (Flow v -> v) ->
   (Flow v -> v) ->
   (idx node -> Idx.StructureEdge node) ->
   Idx.InSection idx node -> Graph node a v -> Maybe v
lookupStruct fieldOut fieldIn unpackIdx =
   withTopology $ lookupStructTopology fieldOut fieldIn unpackIdx

lookupStructTopology ::
   Ord node =>
   (Flow v -> v) ->
   (Flow v -> v) ->
   (idx -> Idx.StructureEdge node) ->
   idx -> Topology node a v -> Maybe v
lookupStructTopology fieldOut fieldIn unpackIdx =
   \idx topo ->
      case unpackIdx idx of
         se ->
            mplus
               (Quant.lookupEdge fieldOut se topo)
               (Quant.lookupEdge fieldIn (Idx.flip se) topo)


lookupEta :: (Ord node) => SeqIdx.Eta node -> Graph node a v -> Maybe v
lookupEta =
   withTopology $ \(Idx.Eta se) -> Quant.lookupEdge flowEta se

lookupSum :: (Ord node) => SeqIdx.Sum node -> Graph node a v -> Maybe v
lookupSum =
   withTopology $ \(Idx.Sum dir node) topo -> do
      sums <- Graph.lookupNode node topo
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


lookupDTime :: SeqIdx.DTime node -> Graph node a v -> Maybe v
lookupDTime (Idx.InPart sec Idx.DTime) =
   fmap fst . seqLookup sec


lookupStorage ::
   (Ord node) => SeqIdx.Storage node -> Graph node a v -> Maybe a
lookupStorage (Idx.ForNode (Idx.Storage bnd) node) g = do
   (_,stores,_) <- Map.lookup node $ storages g
   Map.lookup bnd stores

lookupMaxEnergy ::
   (Ord node) => SeqIdx.MaxEnergy node -> Graph node a v -> Maybe a
lookupMaxEnergy (Idx.ForNode (Idx.MaxEnergy se) node) g = do
   (_,_,edges) <- Map.lookup node $ storages g
   fmap carryMaxEnergy $ Map.lookup se edges

lookupStEnergy ::
   (Ord node) => SeqIdx.StEnergy node -> Graph node a v -> Maybe a
lookupStEnergy (Idx.ForNode (Idx.StEnergy se) node) g = do
   (_,_,edges) <- Map.lookup node $ storages g
   fmap carryEnergy $ Map.lookup se edges

lookupStX ::
   (Ord node) => SeqIdx.StX node -> Graph node a v -> Maybe a
lookupStX (Idx.ForNode (Idx.StX se) node) g = do
   (_,_,edges) <- Map.lookup node $ storages g
   Idx.withStorageEdgeFromTrans
      (fmap carryXIn  . flip Map.lookup edges)
      (fmap carryXOut . flip Map.lookup edges)
      se

lookupStInSum ::
   (Ord node) => SeqIdx.StInSum node -> Graph node a v -> Maybe a
lookupStInSum (Idx.ForNode (Idx.StInSum aug) node) g =
   case aug of
      Idx.Exit -> do
         ((_,exit),_,_) <- Map.lookup node $ storages g
         return exit
      Idx.NoExit sec ->
         fmap carrySum . sumOut =<< lookupSums (Idx.secNode sec node) g

lookupStOutSum ::
   (Ord node) => SeqIdx.StOutSum node -> Graph node a v -> Maybe a
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
   Graph.lookupNode node . snd <=< seqLookup sec

seqLookup ::
   Idx.Section -> Graph node a v -> Maybe (v, Topology node a v)
seqLookup sec = Sequ.lookup sec . sequence


class
   (Env.AccessPart (Env.Environment idx), Var.Index idx, Var.FormatIndex idx) =>
      Lookup idx where
   lookup ::
      (Ord node) =>
      idx node -> Graph node a v -> Maybe (Env.Element idx a v)

instance
   (LookupSignal idx, FormatValue.FormatSignalIndex idx) =>
      Lookup (Idx.InSection idx) where
   lookup = lookupSignal

instance
   (LookupScalar idx, FormatValue.FormatScalarIndex idx) =>
      Lookup (Idx.ForNode idx) where
   lookup = lookupScalar


class
   (Var.SignalIndex idx, FormatValue.FormatSignalIndex idx) =>
      LookupSignal idx where
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


class
   (Var.ScalarIndex idx, FormatValue.FormatScalarIndex idx) =>
      LookupScalar idx where
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


class Unknown a where
   unknown :: a

data Irrelevant = Irrelevant

instance Unknown Irrelevant where
   unknown = Irrelevant

instance Unknown (Result a) where
   unknown = Undetermined

instance Unknown a => Unknown (Record.Absolute a) where
   unknown = pure unknown

instance Unknown a => Unknown (Record.Delta a) where
   unknown = pure unknown

instance (Applicative rec, Unknown a) => Unknown (Record.ExtDelta rec a) where
   unknown = pure unknown


graphFromPlain ::
   (Ord node, Unknown a, Unknown v) =>
   SeqFlow.RangeGraph node -> Graph node a v
graphFromPlain g =
   SeqFlow.Graph {
      storages = storagesFromPlain $ SeqFlow.storages g,
      sequence = sequenceFromPlain $ SeqFlow.sequence g
   }


storagesFromPlain ::
   (Ord node, Unknown a) =>
   SeqFlow.Storages node SeqFlow.InitIn SeqFlow.ExitOut () () ->
   Storages node a
storagesFromPlain =
   Map.map $
      \(_initExit, bnds, edges) ->
         ((unknown, unknown),
          unknown <$ bnds,
          pure unknown <$ edges)


sequenceFromPlain ::
   (Ord node, Unknown a, Unknown v) =>
   SeqFlow.Sequence node Graph.EitherEdge ()
      (Node.Type (Maybe Topo.StoreDir)) () ->
   Sequence node a v
sequenceFromPlain =
   Map.map $ \(rng, ((), gr)) ->
      (,) rng $
      (unknown,
       unknownTopologyNodes $
       Graph.mapEdgeWithKey
          (\ee _ ->
             case ee of
                Graph.EUnDirEdge _ -> Nothing
                Graph.EDirEdge _ -> Just $ pure unknown) gr)

unknownTopologyNodes ::
   (Ord node, Unknown a, Unknown v) =>
   Graph.Graph node Graph.EitherEdge nl el ->
   Graph.Graph node Graph.EitherEdge (Sums a v) el
unknownTopologyNodes =
   Graph.mapNodeWithInOut
      (\(pre, _, suc) ->
         let maybeDir es =
                toMaybe (any (Topo.isActive . fst) es) $
                Sum unknown unknown
         in  Sums {sumIn = maybeDir pre, sumOut = maybeDir suc})

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
   Map.mapWithKey $ \node ((init, exit), bnds, edges) ->
      ((f (Idx.StOutSum Idx.Init <#> node) init,
        f (Idx.StInSum  Idx.Exit <#> node) exit),
       Map.mapWithKey
          (\bnd a -> f (Idx.Storage bnd <#> node) a)
          bnds,
       Map.mapWithKey (mapCarryWithVar f node) edges)

mapCarryWithVar ::
   (Var.ForNodeSectionScalar node -> a0 -> a1) ->
   node -> SeqIdx.StorageEdge node -> Carry a0 -> Carry a1
mapCarryWithVar f node edge =
   liftA2 f (Idx.ForNode <$> (carryVars <*> pure edge) <*> pure node)

carryVars :: Carry (SeqIdx.StorageEdge node -> Var.Scalar Idx.Section node)
carryVars =
   Carry {
      carryMaxEnergy = Var.scalarIndex . Idx.MaxEnergy,
      carryEnergy = Var.scalarIndex . Idx.StEnergy,
      carryXOut = Var.scalarIndex . Idx.StX . Idx.storageTransFromEdge,
      carryXIn = Var.scalarIndex . Idx.StX . Idx.flip . Idx.storageTransFromEdge
   }

mapSequenceWithVar ::
   (Ord node) =>
   (Var.ForNodeSectionScalar node -> a0 -> a1) ->
   (Var.InSectionSignal node -> v0 -> v1) ->
   Sequence node a0 v0 ->
   Sequence node a1 v1
mapSequenceWithVar f g =
   Map.mapWithKey $ \sec (rng, timeGr) ->
      (rng, Quant.mapFlowTopologyWithVar f g sec timeGr)


formatAssigns ::
   (Node.C node, FormatValue a, FormatValue v, Format output) =>
   Graph node a v -> [output]
formatAssigns =
   foldMap (:[]) (:[]) .
   mapGraphWithVar formatAssign formatAssign

foldMap ::
   (Node.C node, Monoid w) =>
   (a -> w) -> (v -> w) -> Graph node a v -> w
foldMap fa fv =
   fold . mapGraph fa fv

fold ::
   (Node.C node, Monoid w) =>
   Graph node w w -> w
fold = MW.execWriter . traverseGraph MW.tell MW.tell
