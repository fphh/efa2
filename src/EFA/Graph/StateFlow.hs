{-# LANGUAGE TypeFamilies #-}
module EFA.Graph.StateFlow where

import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Graph.StateFlow.Environment as StateEnv
import qualified EFA.Graph.StateFlow.Index as StateIdx
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr
import EFA.Graph.Topology
          (FlowTopology, ClassifiedTopology, StateFlowGraph)

import EFA.Equation.Arithmetic ((~+), (~/))
import EFA.Equation.Result (Result)

import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.SequenceData (SequData)

import qualified EFA.Utility.Map as UMap

import qualified Control.Monad.Trans.State as MS

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Stream as Stream; import Data.Stream (Stream)

import qualified Data.Foldable as Fold
import Control.Applicative (liftA2, (<|>))
import Data.Traversable (traverse)
import Data.Foldable (foldMap, fold)
import Data.Tuple.HT (mapPair)



type Topology node nodeLabel = Gr.Graph node Gr.EitherEdge nodeLabel ()

states ::
   (Ord node, Ord nodeLabel) =>
   SequData (Topology node nodeLabel) ->
   Map (Topology node nodeLabel) Idx.State
states =
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
                fmap (UMap.checkedLookup "cumulate StInSumMap" secMap) aug)
       outSumMap =
          flip (cumulateScalarMap add) sos
             (\(Idx.StOutSum aug) ->
                Idx.StOutSum $
                fmap (UMap.checkedLookup "cumulate StOutSumMap" secMap) aug)
   in  StateEnv.Scalar
          eMap (stXMap divide inSumMap outSumMap eMap)
          inSumMap outSumMap

mapStorageEdge ::
   (Ord sec, Show sec, Show state) =>
   String -> Map sec state ->
   Idx.StorageEdge sec node -> Idx.StorageEdge state node
mapStorageEdge caller secMap (Idx.StorageEdge from to) =
   Idx.StorageEdge
      (fmap (UMap.checkedLookup (caller ++ " from") secMap) from)
      (fmap (UMap.checkedLookup (caller ++ " to")   secMap) to)

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
      (\(Idx.InSection sec idx) ->
         Idx.InState (UMap.checkedLookup "cumulateSignalMap" secMap sec) idx)

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
      (Map.mapKeys (\(Idx.InState state Idx.DTime) -> state) dtMap)
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
      (\(Idx.InState state (Idx.Energy (Idx.StructureEdge from _to))) e ->
          {-
          If both In and Out sum are present, then they must be equal.
          -}
          case Map.lookup (Idx.InState state (Idx.Sum Idx.In  from)) sumMap
               <|>
               Map.lookup (Idx.InState state (Idx.Sum Idx.Out from)) sumMap of
             Nothing -> error "StateFlow.xMap: unavailable Sum value"
             Just s -> divide e s)


stateFlow ::
   (Ord node, Ord nodeLabel, Arith.Product a) =>
   SequData (Topology node nodeLabel) ->
   Env.Complete node a a ->
   (Map Idx.State (Topology node nodeLabel),
    StateEnv.Complete node a a)
stateFlow sq env =
   case stateMaps sq of
      (stateMap, secMap) ->
         (stateMap, envFromSequenceEnv secMap env)



stateFromClassTopo ::
  (Ord node) =>
  Idx.State -> ClassifiedTopology node -> StateFlowGraph node
stateFromClassTopo state =
   Gr.ixmap
      (Idx.TimeNode (Idx.augment state))
      (TD.FlowEdge . TD.StructureEdge . Idx.InState state)


storageEdges ::
   Map Idx.State TD.StoreDir -> [Idx.StorageEdge Idx.State node]
storageEdges stores =
   case Map.partition (TD.In ==) stores of
      (ins, outs) ->
         liftA2 Idx.StorageEdge
            (Idx.Init : map Idx.NoInit (Map.keys ins))
            (Idx.Exit : map Idx.NoExit (Map.keys outs))

getStorageSequences ::
   (Ord node, Show node) =>
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


{- |
Insert all possible storage edges.
-}
stateGraphAllStorageEdges ::
   (Ord node, Show node) =>
   SequData (FlowTopology node) ->
   StateFlowGraph node
stateGraphAllStorageEdges sd =
   Flow.insEdges (fmap (storageEdges . Map.mapMaybe id) tracks) $
   Flow.insNodes (Map.keys tracks) $
   Fold.fold $ Map.mapWithKey stateFromClassTopo sq
  where sq = fmap TD.classifyStorages $ fst $ stateMaps sd
        tracks = getStorageSequences sq

{- |
Insert only the storage edges that have counterparts in the sequence flow graph.
-}
stateGraphActualStorageEdges ::
   (Ord node, Show node) =>
   SequData (FlowTopology node) ->
   StateFlowGraph node
stateGraphActualStorageEdges sd =
   Flow.insEdges
      (fmap (map (mapStorageEdge "stateGraph" secMap) .
             Flow.storageEdges . Map.mapMaybe id) tracks) $
   Flow.insNodes (Map.keys tracks) $
   Fold.fold $ Map.mapWithKey stateFromClassTopo stateMap
  where sq = fmap TD.classifyStorages sd
        (stateMap, secMap) = stateMaps sq
        tracks = Flow.getStorageSequences sq
