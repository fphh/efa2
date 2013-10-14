{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module EFA.Flow.Sequence.Quantity (
   Graph, SeqFlow.sequence, SeqFlow.storages,
   Topology, Sequence, Storages,
   Sums(..), Carry(..), Flow(..),

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

{-
   envFromGraph,
-}
   graphFromEnv,
   toAssignMap,

   Unknown(..),
   graphFromPlain,
   storagesFromPlain,
   sequenceFromPlain,

   mapGraphWithVar,
   mapStoragesWithVar,
   mapSequenceWithVar,

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
   Env.Environment, Env.Element, Env.switchPart,

   formatAssigns,
   ) where

import qualified EFA.Flow.Sequence.AssignMap as AssignMap
import qualified EFA.Flow.Sequence.Index as SeqIdx
import qualified EFA.Flow.Sequence as SeqFlow
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Flow.StorageGraph.Quantity as StorageQuant
import qualified EFA.Flow.StorageGraph as StorageGraph
import qualified EFA.Flow.PartMap as PartMap
import EFA.Flow.Topology.Quantity (Topology, Sums(..), Flow(..))
import EFA.Flow.StorageGraph (StorageGraph(StorageGraph))
import EFA.Flow.Sequence.AssignMap (AssignMap)
import EFA.Flow.Sequence (sequence, storages)

import qualified EFA.Signal.Sequence as Sequ

import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Variable as Var
import EFA.Equation.Unknown (Unknown(unknown))
import EFA.Equation.Variable ((<#>))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import EFA.Report.FormatValue (FormatValue, formatAssign)
import EFA.Report.Format (Format)

import qualified EFA.Utility.Map as MapU
import EFA.Utility.Map (Caller)

import qualified Control.Monad.Trans.Writer as MW
import Control.Monad (mplus, (<=<))
import Control.Applicative (Applicative, pure, liftA2, (<*>), (<$))

import qualified Data.Map as Map
import qualified Data.Foldable as Fold

import Data.Traversable (Traversable, traverse, foldMapDefault)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid)
import Data.Tuple.HT (mapPair, mapSnd)

import Prelude hiding (lookup, init, seq, sequence, sin, sum)


type
   Storages node a = SeqFlow.Storages node a a (Carry a)

type
   Sequence node v =
      SeqFlow.Sequence node Graph.EitherEdge v (Sums v) (Maybe (Flow v))

type
   Graph node a v =
      SeqFlow.Graph node Graph.EitherEdge
         v (Sums v) a a (Maybe (Flow v)) (Carry a)

data Carry a =
   Carry {
      carryMaxEnergy, carryEnergy, carryXOut, carryXIn :: a
   }
   deriving (Eq)


instance StorageQuant.Carry Carry where
   carryEnergy = carryEnergy
   carryXOut   = carryXOut
   carryXIn    = carryXIn

   type CarryPart Carry = Idx.Section
   carryVars =
      Carry {
         carryMaxEnergy = Var.scalarIndex . Idx.MaxEnergy,
         carryEnergy = Var.scalarIndex . Idx.StEnergy,
         carryXOut = Var.scalarIndex . Idx.StX . Idx.storageTransFromEdge,
         carryXIn = Var.scalarIndex . Idx.StX . Idx.flip . Idx.storageTransFromEdge
      }


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
      sequence = mapSequence g $ sequence gr,
      storages = mapStorages f $ storages gr
   }

mapSequence ::
   (v0 -> v1) ->
   Sequence node v0 -> Sequence node v1
mapSequence f =
   fmap (mapSnd $ FlowTopo.mapSection f)

mapStorages ::
   (a0 -> a1) ->
   Storages node a0 -> Storages node a1
mapStorages f =
   fmap
      (mapPair
         (StorageGraph.mapNode f .
          StorageGraph.mapEdge (fmap f),
           fmap f))


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
      sequence = checkedZipWithSequence caller g (sequence gr0) (sequence gr1),
      storages = checkedZipWithStorages caller f (storages gr0) (storages gr1)
   }

checkedZipWithSequence ::
   (Ord node) =>
   Caller ->
   (v0 -> v1 -> v2) ->
   Sequence node v0 ->
   Sequence node v1 ->
   Sequence node v2
checkedZipWithSequence caller f =
   MapU.checkedZipWith (caller++".checkedZipWithSequence")
      (\(rng0, gr0) (rng1, gr1) ->
         (if rng0==rng1 then rng0 else error (caller++".equalRange"),
          FlowTopo.checkedZipWithSection
             (caller++".checkedZipWithSequence.section")
             f gr0 gr1))

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
          (\(graph0, storage0)
            (graph1, storage1) ->
               (StorageGraph.checkedZipWith name f (liftA2 f) graph0 graph1,
                MapU.checkedZipWith (name++".storage") f storage0 storage1))


traverseGraph ::
   (Applicative f, Ord node) =>
   (a0 -> f a1) ->
   (v0 -> f v1) ->
   Graph node a0 v0 -> f (Graph node a1 v1)
traverseGraph f g (SeqFlow.Graph sts seq) =
   liftA2 SeqFlow.Graph
      (traverseStorages f $ sts)
      (traverseSequence g $ seq)

traverseSequence ::
   (Applicative f, Ord node) =>
   (v0 -> f v1) ->
   Sequence node v0 -> f (Sequence node v1)
traverseSequence f =
   traverse
      (\(rng, gr) -> fmap ((,) rng) $ FlowTopo.traverseSection f gr)

traverseStorages ::
   (Applicative f, Ord node) =>
   (a0 -> f a1) ->
   Storages node a0 -> f (Storages node a1)
traverseStorages f =
   traverse
      (uncurry (liftA2 (,)) .
       mapPair (StorageGraph.traverse f (traverse f), traverse f))


{-
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
             els = Graph.edgeLabels $ FlowTopoPlain.dirFromFlowGraph topo
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
-}


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
   idx -> Topology node v -> Maybe v
lookupStructTopology fieldOut fieldIn unpackIdx =
   \idx topo ->
      case unpackIdx idx of
         se ->
            mplus
               (FlowTopoPlain.lookupEdge fieldOut se topo)
               (FlowTopoPlain.lookupEdge fieldIn (Idx.flip se) topo)


lookupEta :: (Ord node) => SeqIdx.Eta node -> Graph node a v -> Maybe v
lookupEta =
   withTopology $ \(Idx.Eta se) -> FlowTopoPlain.lookupEdge flowEta se

lookupSum :: (Ord node) => SeqIdx.Sum node -> Graph node a v -> Maybe v
lookupSum =
   withTopology $ \(Idx.Sum dir node) topo -> do
      sums <- Graph.lookupNode node topo
      case dir of
         Idx.In  -> sumIn sums
         Idx.Out -> sumOut sums


withTopology ::
   (idx node -> Topology node v -> Maybe r) ->
   Idx.InSection idx node ->
   Graph node a v ->
   Maybe r
withTopology f (Idx.InPart sec idx) g =
   f idx . FlowTopo.topology =<< seqLookup sec g


lookupDTime :: SeqIdx.DTime node -> Graph node a v -> Maybe v
lookupDTime (Idx.InPart sec Idx.DTime) =
   fmap FlowTopo.label . seqLookup sec


lookupStorage ::
   (Ord node) => SeqIdx.Storage node -> Graph node a v -> Maybe a
lookupStorage (Idx.ForNode (Idx.Storage bnd) node) g = do
   (_,stores) <- Map.lookup node $ storages g
   Map.lookup bnd stores

lookupMaxEnergy ::
   (Ord node) => SeqIdx.MaxEnergy node -> Graph node a v -> Maybe a
lookupMaxEnergy (Idx.ForNode (Idx.MaxEnergy se) node) g = do
   (sgr,_) <- Map.lookup node $ storages g
   fmap carryMaxEnergy $ StorageGraph.lookupEdge se sgr

lookupStEnergy ::
   (Ord node) => SeqIdx.StEnergy node -> Graph node a v -> Maybe a
lookupStEnergy (Idx.ForNode (Idx.StEnergy se) node) g = do
   (sgr,_) <- Map.lookup node $ storages g
   fmap carryEnergy $ StorageGraph.lookupEdge se sgr

lookupStX ::
   (Ord node) => SeqIdx.StX node -> Graph node a v -> Maybe a
lookupStX (Idx.ForNode (Idx.StX se) node) g = do
   (sgr,_) <- Map.lookup node $ storages g
   Idx.withStorageEdgeFromTrans
      (fmap carryXIn  . flip StorageGraph.lookupEdge sgr)
      (fmap carryXOut . flip StorageGraph.lookupEdge sgr)
      se

{- |
It is an unchecked error if you lookup StInSum where is only an StOutSum.
-}
lookupStInSum ::
   (Ord node) => SeqIdx.StInSum node -> Graph node a v -> Maybe a
lookupStInSum (Idx.ForNode (Idx.StInSum aug) node) g = do
   (StorageGraph partMap _, _) <- Map.lookup node $ storages g
   case aug of
      Idx.Exit -> return $ PartMap.exit partMap
      Idx.NoExit sec -> Map.lookup sec $ PartMap.parts partMap

{- |
It is an unchecked error if you lookup StOutSum where is only an StInSum.
-}
lookupStOutSum ::
   (Ord node) => SeqIdx.StOutSum node -> Graph node a v -> Maybe a
lookupStOutSum (Idx.ForNode (Idx.StOutSum aug) node) g = do
   (StorageGraph partMap _, _) <- Map.lookup node $ storages g
   case aug of
      Idx.Init -> return $ PartMap.init partMap
      Idx.NoInit sec -> Map.lookup sec $ PartMap.parts partMap

lookupSums ::
   (Ord node) =>
   Idx.SecNode node -> Graph node a v -> Maybe (Sums v)
lookupSums (Idx.PartNode sec node) =
   Graph.lookupNode node . FlowTopo.topology <=< seqLookup sec

seqLookup ::
   Idx.Section -> Graph node a v -> Maybe (FlowTopo.Section node v)
seqLookup sec = Sequ.lookup sec . sequence


class
   (Env.AccessPart (Env.Environment idx), Var.Index idx, Var.FormatIndex idx) =>
      Lookup idx where
   lookup ::
      (Ord node) =>
      idx node -> Graph node a v -> Maybe (Env.Element idx a v)

instance
   (LookupSignal idx, Var.SignalIndex idx) =>
      Lookup (Idx.InSection idx) where
   lookup = lookupSignal

instance
   (LookupScalar idx, Var.ScalarIndex idx) =>
      Lookup (Idx.ForNode idx) where
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


data Irrelevant = Irrelevant

instance Unknown Irrelevant where
   unknown = Irrelevant


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
   SeqFlow.Storages node () () () ->
   Storages node a
storagesFromPlain =
   Map.map $
      mapPair
         (StorageGraph.mapNode (const unknown) .
          StorageGraph.mapEdge (const $ pure unknown),
          (unknown <$))


sequenceFromPlain ::
   (Ord node, Unknown v) =>
   SeqFlow.Sequence node Graph.EitherEdge ()
      (Node.Type (Maybe Topo.StoreDir)) () ->
   Sequence node v
sequenceFromPlain =
   Map.map $ mapSnd $ FlowTopo.sectionFromPlain

mapGraphWithVar ::
   (Ord node) =>
   (Var.ForNodeSectionScalar node -> a0 -> a1) ->
   (Var.InSectionSignal node -> v0 -> v1) ->
   Graph node a0 v0 ->
   Graph node a1 v1
mapGraphWithVar f g gr =
   SeqFlow.Graph {
      storages = mapStoragesWithVar f gr,
      sequence = mapSequenceWithVar g $ sequence gr
   }

mapStoragesWithVar ::
   (Ord node) =>
   (Var.ForNodeSectionScalar node -> a0 -> a1) ->
   Graph node a0 v0 ->
   Storages node a1
mapStoragesWithVar f gr =
   Map.mapWithKey
      (\node (sgr, bnds) ->
         (StorageQuant.mapGraphWithVar (flip lookupSums gr) f node sgr,
          Map.mapWithKey
             (\bnd a -> f (Idx.Storage bnd <#> node) a)
             bnds)) $
   storages gr

mapSequenceWithVar ::
   (Ord node) =>
   (Var.InSectionSignal node -> v0 -> v1) ->
   Sequence node v0 ->
   Sequence node v1
mapSequenceWithVar f =
   Map.mapWithKey $ \sec ->
      mapSnd $ FlowTopo.mapSectionWithVar (f . Idx.InPart sec)


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
