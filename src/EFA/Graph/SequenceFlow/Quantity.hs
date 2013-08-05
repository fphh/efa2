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
   ) where

import qualified EFA.Application.Index as XIdx

import qualified EFA.Equation.Environment as Env

import qualified EFA.Graph.SequenceFlow as SeqFlow
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr

import qualified EFA.Signal.SequenceData as SD

import qualified EFA.Utility.Map as MapU

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
   let lookup caller idx m =
          fromMaybe (error $ "storagesFromEnv: " ++ caller) $
          Map.lookup idx m

   in  MapU.checkedZipWith "storagesFromEnv"
          (\stores (sums, edges) -> (sums, stores, edges))
          (fmap (Map.mapKeys (\(Idx.Storage store) -> store)) $
           curryNodeMap $ Env.storageMap env)
       .
       (Map.mapWithKey $ \node (_initExit, edges) ->
          ((lookup "stOutSum" (XIdx.stOutSum XIdx.initSection node) $
            Env.stOutSumMap env,
            lookup "stInSum"  (XIdx.stInSum  XIdx.exitSection node) $
            Env.stInSumMap  env),
           Map.mapWithKey
              (\edge () ->
                 Carry {
                    carryMaxEnergy =
                       lookup "carryMaxEnergy"
                          (Idx.ForNode (Idx.MaxEnergy edge) node) $
                       Env.maxEnergyMap env,
                    carryEnergy =
                       lookup "carryEnergy"
                          (Idx.ForNode (Idx.StEnergy edge) node) $
                       Env.stEnergyMap env,
                    carryXOut =
                       lookup "carryXOut"
                          (Idx.ForNode (Idx.StX $ Idx.storageTransFromEdge edge) node) $
                       Env.stXMap env,
                    carryXIn =
                       lookup "carryXIn"
                          (Idx.ForNode (Idx.StX $ Idx.flip $ Idx.storageTransFromEdge edge) node) $
                       Env.stXMap env
                 })
              edges))

curryNodeMap ::
   (Ord node, Ord (idx node)) =>
   Map (Idx.ForNode idx node) a -> Map node (Map (idx node) a)
curryNodeMap =
   Map.unionsWith (Map.unionWith
      (error "SequenceFlow.Quantity.curryNodeMap: duplicate key")) .
   Map.elems .
   Map.mapWithKey
      (\(Idx.ForNode idx node) a ->
         Map.singleton node $ Map.singleton idx a)


sequenceFromEnv ::
   (Ord node) =>
   ((Env.StInSumMap node a, Env.StOutSumMap node a),
    Env.Signal node v) ->
   SeqFlow.Sequence node Gr.DirEdge nodeType () ->
   Sequence node a v
sequenceFromEnv ((stInSumMap, stOutSumMap), env) =
   let lookup caller idx m =
          fromMaybe (error $ "sequenceFromEnv: " ++ caller) $
          Map.lookup idx m

       lookupFlow caller sec e idx =
          lookup caller
             (Idx.InPart sec $ idx $
              Topo.structureEdgeFromDirEdge e) $
          Acc.get Env.accessSignalMap env

   in  SD.mapWithSection $ \sec g ->
          (lookup "dtime" (XIdx.dTime sec) $ Env.dtimeMap env,
           Gr.nmapWithKey
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
           Gr.emapWithKey
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
