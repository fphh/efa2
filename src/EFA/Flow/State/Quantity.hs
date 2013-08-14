{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.State.Quantity where

import qualified EFA.Flow.Quantity as Quant
import qualified EFA.Flow.Sequence.Quantity as SeqFlowQuant
import qualified EFA.Flow.State as StateFlow
import EFA.Flow.State (states, storages)
import EFA.Flow.Quantity
          (Sums(..), Sum(..), Flow(..), mapSums, traverseSums)

import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Graph.StateFlow.Environment as StateEnv
import qualified EFA.Graph.StateFlow.Index as StateIdx
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr
import EFA.Graph.Topology (ClassifiedTopology, StateFlowGraph)

import EFA.Equation.Arithmetic ((~+), (~/))
import EFA.Equation.Result (Result)

import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.SequenceData (SequData)

import qualified EFA.Utility.Map as MapU

import qualified Control.Monad.Trans.State as MS

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Stream as Stream; import Data.Stream (Stream)

import qualified Data.Foldable as Fold
import Control.Applicative (Applicative, pure, liftA2, (<*>), (<|>))
import Data.Traversable (Traversable, traverse, foldMapDefault)
import Data.Foldable (Foldable, foldMap, fold)
import Data.Tuple.HT (mapSnd, mapPair)
import Data.Maybe (isJust, fromMaybe)

import Prelude hiding (lookup, init, seq, sequence, sin, sum)



type
   Storages node a = StateFlow.Storages node a a (Carry a)

type
   States node a v = StateFlow.States node Gr.DirEdge v (Sums a v) (Flow v)

type
   Graph node a v =
      StateFlow.Graph node Gr.DirEdge
         v (Sums a v) a a (Flow v) (Carry a)

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
          Gr.mapNode (mapSums f g) $
          Gr.mapEdge (fmap g) gr))

mapStorages ::
   (a0 -> a1) ->
   Storages node a0 -> Storages node a1
mapStorages f =
   fmap
      (\((init, exit), edges) ->
         ((f init, f exit),
          fmap (fmap f) edges))


traverseGraph ::
   (Applicative f) =>
   (a0 -> f a1) ->
   (v0 -> f v1) ->
   Graph node a0 v0 -> f (Graph node a1 v1)
traverseGraph f g (StateFlow.Graph sts seq) =
   liftA2 StateFlow.Graph
      (traverseStorages f   $ sts)
      (traverseStates   f g $ seq)

traverseStates ::
   (Applicative f) =>
   (a0 -> f a1) ->
   (v0 -> f v1) ->
   States node a0 v0 -> f (States node a1 v1)
traverseStates f g =
   traverse
      (\(dt, gr) ->
         liftA2 (,) (g dt)
            (Gr.traverse (traverseSums f g) (traverse g) gr))

traverseStorages ::
   (Applicative f) =>
   (a0 -> f a1) ->
   Storages node a0 -> f (Storages node a1)
traverseStorages f =
   traverse
      (\((init, exit), edges) ->
         liftA2 (,)
            (liftA2 (,) (f init) (f exit))
            (traverse (traverse f) edges))



type Topology node nodeLabel = Gr.Graph node Gr.EitherEdge nodeLabel ()

_states ::
   (Ord node, Ord nodeLabel) =>
   SequData (Topology node nodeLabel) ->
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
   SequData (Topology node nodeLabel) ->
   (Map Idx.State (Topology node nodeLabel),
    Map Idx.Section Idx.State)
stateMaps sq =
   mapPair (fold, fold) $ unzip $ Fold.toList $
   flip MS.evalState (Map.empty, Stream.iterate succ $ Idx.State 0) $
   traverse
      (\(sec,g) -> do
         i <- identify g
         return (Map.singleton i g, Map.singleton sec i)) $
   SD.mapWithSection (,) sq


type
   CumGraph node a =
      StateFlow.Graph node Gr.DirEdge a (Sums a a) a a (Cum a) a


fromSequenceFlowGen ::
   (Ord node) =>
   (v -> a) ->
   (a -> a -> a) ->
   a ->
   Bool ->
   Map Idx.Section Idx.State ->
   SeqFlowQuant.Graph node a v ->
   CumGraph node a
fromSequenceFlowGen integrate add zero allStEdges secMap gr =
   let sts =
          flip cumulateSequence secMap
             (\(dtime0, gr0) (dtime1, gr1) ->
                (add dtime0 dtime1,
                 Gr.checkedZipWith "StateFlow.fromSequenceFlowActualSE"
                    (addSums add)
                    (liftA2 add)
                    gr0 gr1)) $
          fmap (mapSnd $ Gr.mapEdge $ cumFromFlow) $
          fmap snd $
          SeqFlowQuant.mapSequence id integrate $
          SeqFlowQuant.sequence gr
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
   StateFlow.States node Gr.DirEdge v (Sums a v) (Cum v) ->
   Map Idx.State (Sums a v)
sumsMap node =
   fmap (fromMaybe (error "node not in sequence") .
         Gr.lookupNode node . snd)

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



envFromSequenceEnv ::
   (Ord node, Arith.Product a) =>
   Map Idx.Section Idx.State ->
   Env.Complete node a a ->
   StateEnv.Complete node a a
envFromSequenceEnv secMap (Env.Complete scalar signal) =
   StateEnv.Complete
      (scalarEnvFromSequenceEnv (~/) (~+) secMap scalar)
      (signalEnvFromSequenceEnv (~/) (~+) secMap signal)

envFromSequenceEnvResult ::
   (Ord node, Arith.Product a) =>
   Map Idx.Section Idx.State ->
   Env.Complete node (Result a) (Result a) ->
   StateEnv.Complete node (Result a) (Result a)
envFromSequenceEnvResult secMap (Env.Complete scalar signal) =
   StateEnv.Complete
      (scalarEnvFromSequenceEnv (liftA2 (~/)) (liftA2 (~+)) secMap scalar)
      (signalEnvFromSequenceEnv (liftA2 (~/)) (liftA2 (~+)) secMap signal)

scalarEnvFromSequenceEnv ::
   (Ord node) =>
   (a -> a -> a) ->
   (a -> a -> a) ->
   Map Idx.Section Idx.State ->
   Env.Scalar node a ->
   StateEnv.Scalar node a
scalarEnvFromSequenceEnv divide add secMap (Env.Scalar _me _st se _sx sis sos) =
   let eMap =
          flip (cumulateScalarMap add) se
             (\(Idx.StEnergy e) ->
                Idx.StEnergy $ mapStorageEdge "cumulate StEnergyMap" secMap e)
       inSumMap =
          flip (cumulateScalarMap add) sis
             (\(Idx.StInSum aug) ->
                Idx.StInSum $
                fmap (MapU.checkedLookup "cumulate StInSumMap" secMap) aug)
       outSumMap =
          flip (cumulateScalarMap add) sos
             (\(Idx.StOutSum aug) ->
                Idx.StOutSum $
                fmap (MapU.checkedLookup "cumulate StOutSumMap" secMap) aug)
   in  StateEnv.Scalar
          eMap (stXMap divide inSumMap outSumMap eMap)
          inSumMap outSumMap

mapStorageEdge ::
   (Ord sec, Show sec, Show state) =>
   String -> Map sec state ->
   Idx.StorageEdge sec node -> Idx.StorageEdge state node
mapStorageEdge caller secMap (Idx.StorageEdge from to) =
   Idx.StorageEdge
      (fmap (MapU.checkedLookup (caller ++ " from") secMap) from)
      (fmap (MapU.checkedLookup (caller ++ " to")   secMap) to)

cumulateScalarMap ::
   (Ord node, Ord (stateIdx node)) =>
   (a -> a -> a) ->
   (secIdx node -> stateIdx node) ->
   Map (Idx.ForNode secIdx node) a ->
   Map (Idx.ForNode stateIdx node) a
cumulateScalarMap add f =
   Map.mapKeysWith add
      (\(Idx.ForNode aug node) -> Idx.ForNode (f aug) node)

stXMap ::
   (Ord node) =>
   (a -> a -> a) ->
   Map (StateIdx.StInSum node) a ->
   Map (StateIdx.StOutSum node) a ->
   Map (StateIdx.StEnergy node) a ->
   Map (StateIdx.StX node) a
stXMap divide inSumMap outSumMap =
   fold .
   Map.mapWithKey
      (\(Idx.ForNode (Idx.StEnergy edge@(Idx.StorageEdge from to)) node) e ->
         let stx = Idx.ForNode (Idx.StX (Idx.storageTransFromEdge edge)) node
         in  Map.singleton stx
                (divide e $
                 Map.findWithDefault (error "StateFlow.stXMap from")
                    (Idx.ForNode (Idx.StOutSum from) node) outSumMap)
             `Map.union`
             Map.singleton (Idx.flip stx)
                (divide e $
                 Map.findWithDefault (error "StateFlow.stXMap from")
                    (Idx.ForNode (Idx.StInSum to) node) inSumMap))


signalEnvFromSequenceEnv ::
   (Ord node) =>
   (a -> a -> a) ->
   (a -> a -> a) ->
   Map Idx.Section Idx.State ->
   Env.Signal node a ->
   StateEnv.Signal node a
signalEnvFromSequenceEnv divide add secMap (Env.Signal e _p _n dt _x s) =
   let eState = cumulateSignalMap add secMap e
       dtState = cumulateSignalMap add secMap dt
       sumState = cumulateSignalMap add secMap s
   in  StateEnv.Signal
          eState
          (powerMap divide dtState eState)
          (etaMap divide eState)
          dtState
          (xMap divide sumState eState)
          sumState

cumulateSignalMap ::
   (Ord (idx node)) =>
   (a -> a -> a) ->
   Map Idx.Section Idx.State ->
   Map (Idx.InSection idx node) a ->
   Map (Idx.InState idx node) a
cumulateSignalMap add secMap =
   Map.mapKeysWith add
      (\(Idx.InPart sec idx) ->
         Idx.InPart (MapU.checkedLookup "cumulateSignalMap" secMap sec) idx)

powerMap ::
   (Ord node) =>
   (a -> a -> a) ->
   Map (StateIdx.DTime node) a ->
   Map (StateIdx.Energy node) a ->
   Map (StateIdx.Power node) a
powerMap divide dtMap eMap =
   StateEnv.uncurrySignal $
   Map.intersectionWith
      (\dt ->
         Map.mapKeys (\(Idx.Energy e) -> Idx.Power e) . fmap (flip divide dt))
      (Map.mapKeys (\(Idx.InPart state Idx.DTime) -> state) dtMap)
      (StateEnv.currySignal eMap)

etaMap ::
   (Ord node) =>
   (a -> a -> a) ->
   Map (StateIdx.Energy node) a -> Map (StateIdx.Eta node) a
etaMap divide eMap =
   Map.mapKeys (Idx.liftInState (\(Idx.Energy e) -> Idx.Eta e)) $
   Map.intersectionWith divide (Map.mapKeys Idx.flip eMap) eMap

xMap ::
   (Ord node) =>
   (a -> a -> a) ->
   Map (StateIdx.Sum node) a ->
   Map (StateIdx.Energy node) a ->
   Map (StateIdx.X node) a
xMap divide sumMap =
   Map.mapKeys (Idx.liftInState (\(Idx.Energy e) -> Idx.X e)) .
   Map.mapWithKey
      (\(Idx.InPart state (Idx.Energy (Idx.StructureEdge from _to))) e ->
          {-
          If both In and Out sum are present, then they must be equal.
          -}
          case Map.lookup (Idx.InPart state (Idx.Sum Idx.In  from)) sumMap
               <|>
               Map.lookup (Idx.InPart state (Idx.Sum Idx.Out from)) sumMap of
             Nothing -> error "StateFlow.xMap: unavailable Sum value"
             Just s -> divide e s)



stateFromClassTopo ::
   (Ord node) =>
   Idx.State -> ClassifiedTopology node -> StateFlowGraph node
stateFromClassTopo state =
   Gr.ixmap
      (Idx.PartNode (Idx.augment state))
      (TD.FlowEdge . TD.StructureEdge . Idx.InPart state)


storageEdges ::
   Map Idx.State TD.StoreDir -> [Idx.StorageEdge Idx.State node]
storageEdges stores =
   case Map.partition (TD.In ==) stores of
      (ins, outs) ->
         liftA2 Idx.StorageEdge
            (Idx.Init : map Idx.NoInit (Map.keys ins))
            (Idx.Exit : map Idx.NoExit (Map.keys outs))

getStorageSequences ::
   (Ord node) =>
   Map Idx.State (TD.ClassifiedTopology node) ->
   Map node (Map Idx.State (Maybe TD.StoreDir))
getStorageSequences =
   Map.unionsWith (Map.unionWith (error "duplicate section for node"))
   .
   Map.elems
   .
   Map.mapWithKey
      (\s g ->
         fmap (Map.singleton s) $
         Map.mapMaybe TD.maybeStorage $ Gr.nodeLabels g)



