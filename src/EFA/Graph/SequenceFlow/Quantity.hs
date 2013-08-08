{-# LANGUAGE FlexibleInstances #-}
module EFA.Graph.SequenceFlow.Quantity (
   Graph, Topology, Sequence, Storages,
   Sums, Sum, Carry, Flow,

   envFromGraph,
   envFromStorages,
   envFromSequence,

   graphFromEnv,
   storagesFromEnv,
   sequenceFromEnv,

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
   ) where

import qualified EFA.Application.Index as XIdx

import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Variable as Var

import qualified EFA.Graph.SequenceFlow as SeqFlow
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr

import qualified EFA.Signal.SequenceData as SD

import qualified EFA.Utility.Map as MapU

import Control.Monad (mplus)
import Control.Applicative (Applicative, pure, liftA2, (<*>))

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Foldable as Fold
import qualified Data.Accessor.Basic as Acc

import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, (<>))

import Prelude hiding (lookup, init, sequence)


type
   Storages node a =
      Map node
         ((a, a),
          Map Idx.Boundary a,
          Map (XIdx.StorageEdge node) (Carry a))

type
   Topology node a v =
      Gr.Graph node Gr.DirEdge (Sums a v) (Flow v)

type
   Sequence node a v =
      SD.SequData (v, Topology node a v)

data
   Graph node a v =
      Graph {
         storages :: Storages node a,
         sequence :: Sequence node a v
      }

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
   SD.mapWithSection
      (\sec (dtime, topo) ->
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
graphFromEnv (Env.Complete envScalar envSignal) g =
   Graph {
      storages = storagesFromEnv envScalar $ SeqFlow.storages g,
      sequence =
         sequenceFromEnv
            ((Env.stInSumMap envScalar, Env.stOutSumMap envScalar), envSignal) $
         fmap dirFromFlowGraph $ SeqFlow.sequence g
   }

dirFromFlowGraph ::
   (Ord n) =>
   Gr.Graph n Gr.EitherEdge nl el -> Gr.Graph n Gr.DirEdge nl el
dirFromFlowGraph =
   Gr.mapEdgesMaybe $ \ee ->
      case ee of
         Gr.EDirEdge de -> Just de
         Gr.EUnDirEdge _ -> Nothing


storagesFromEnv ::
   (Ord node) =>
   Env.Scalar node a ->
   SeqFlow.Storages node SeqFlow.InitIn SeqFlow.ExitOut () ->
   Storages node a
storagesFromEnv env =
   let lookupEnv caller idx m =
          fromMaybe (error $ "storagesFromEnv: " ++ caller) $
          Map.lookup idx m

   in  MapU.checkedZipWith "storagesFromEnv"
          (\stores (sums, edges) -> (sums, stores, edges))
          (MapU.curry "SequenceFlow.Quantity.storagesFromEnv"
              (\(Idx.ForNode (Idx.Storage idx) node) -> (node, idx)) $
           Env.storageMap env)
       .
       (Map.mapWithKey $ \node (_initExit, edges) ->
          ((lookupEnv "stOutSum" (XIdx.stOutSum XIdx.initSection node) $
            Env.stOutSumMap env,
            lookupEnv "stInSum"  (XIdx.stInSum  XIdx.exitSection node) $
            Env.stInSumMap  env),
           Map.mapWithKey
              (\edge () ->
                 Carry {
                    carryMaxEnergy =
                       lookupEnv "carryMaxEnergy"
                          (Idx.ForNode (Idx.MaxEnergy edge) node) $
                       Env.maxEnergyMap env,
                    carryEnergy =
                       lookupEnv "carryEnergy"
                          (Idx.ForNode (Idx.StEnergy edge) node) $
                       Env.stEnergyMap env,
                    carryXOut =
                       lookupEnv "carryXOut"
                          (Idx.ForNode (Idx.StX $ Idx.storageTransFromEdge edge) node) $
                       Env.stXMap env,
                    carryXIn =
                       lookupEnv "carryXIn"
                          (Idx.ForNode (Idx.StX $ Idx.flip $ Idx.storageTransFromEdge edge) node) $
                       Env.stXMap env
                 })
              edges))


sequenceFromEnv ::
   (Ord node) =>
   ((Env.StInSumMap node a, Env.StOutSumMap node a),
    Env.Signal node v) ->
   SeqFlow.Sequence node Gr.DirEdge nodeType () ->
   Sequence node a v
sequenceFromEnv ((stInSumMap, stOutSumMap), env) =
   let lookupEnv caller idx m =
          fromMaybe (error $ "sequenceFromEnv: " ++ caller) $
          Map.lookup idx m

       lookupFlow caller sec e idx =
          lookupEnv caller
             (Idx.InPart sec $ idx $
              Topo.structureEdgeFromDirEdge e) $
          Acc.get Env.accessSignalMap env

   in  SD.mapWithSection $ \sec g ->
          (lookupEnv "dtime" (XIdx.dTime sec) $ Env.dtimeMap env,
           Gr.mapNodeWithKey
              (\n _nt ->
                 Sums {
                    sumIn  =
                       liftA2 Sum
                          (Map.lookup (XIdx.stOutSum sec n) stOutSumMap)
                          (Map.lookup (XIdx.inSum sec n) $ Env.sumMap env),
                    sumOut =
                       liftA2 Sum
                          (Map.lookup (XIdx.stInSum sec n) stInSumMap)
                          (Map.lookup (XIdx.outSum sec n) $ Env.sumMap env)
                 }) $
           Gr.mapEdgeWithKey
              (\e () ->
                 Flow {
                    flowPowerOut =
                       lookupFlow "flowPowerOut" sec e Idx.Power,
                    flowPowerIn =
                       lookupFlow "flowPowerIn" sec e (Idx.Power . Idx.flip),
                    flowEnergyOut =
                       lookupFlow "flowEnergyOut" sec e Idx.Energy,
                    flowEnergyIn =
                       lookupFlow "flowEnergyIn" sec e (Idx.Energy . Idx.flip),
                    flowXOut =
                       lookupFlow "flowXOut" sec e Idx.X,
                    flowXIn =
                       lookupFlow "flowXIn" sec e (Idx.X . Idx.flip),
                    flowEta =
                       lookupFlow "flowEta" sec e Idx.Eta
                 })
              g)


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
   f idx . snd =<< SD.lookup sec (sequence g)


lookupDTime :: XIdx.DTime node -> Graph node a v -> Maybe v
lookupDTime (Idx.InPart sec Idx.DTime) =
   fmap fst . SD.lookup sec . sequence


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
         fmap carrySum . sumOut =<<
         Gr.lookupNode node . snd =<< SD.lookup sec (sequence g)

lookupStOutSum ::
   (Ord node) => XIdx.StOutSum node -> Graph node a v -> Maybe a
lookupStOutSum (Idx.ForNode (Idx.StOutSum aug) node) g =
   case aug of
      Idx.Init -> do
         ((init,_),_,_) <- Map.lookup node $ storages g
         return init
      Idx.NoInit sec ->
         fmap carrySum . sumIn =<<
         Gr.lookupNode node . snd =<< SD.lookup sec (sequence g)



class (Var.Index idx) => AccessMap idx where
   lookup ::
      (Ord node) =>
      idx node -> Graph node a v -> Maybe (Env.Element idx a v)

instance (LookupSignal idx) => AccessMap (Idx.InSection idx) where
   lookup = lookupSignal

instance (LookupScalar idx) => AccessMap (Idx.ForNode idx) where
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
