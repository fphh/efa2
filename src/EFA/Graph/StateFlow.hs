{-# LANGUAGE TypeFamilies #-}
module EFA.Graph.StateFlow where

import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Graph.StateFlow.Environment as StateEnv
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr
import EFA.Graph.Topology
          (FlowTopology, ClassifiedTopology, StateFlowGraph)

import EFA.Equation.Arithmetic ((~+))

import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.SequenceData (SequData)

import qualified EFA.Utility.Map as UMap

import qualified Control.Monad.Trans.State as MS

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Stream as Stream; import Data.Stream (Stream)

import qualified Data.Foldable as Fold
import Control.Applicative (liftA2)
import Data.Traversable (traverse)
import Data.Foldable (foldMap, fold)
import Data.Tuple.HT (mapPair)



states ::
   (Ord node) =>
   SequData (TD.FlowTopology node) ->
   Map (TD.FlowTopology node) Idx.State
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
   (Ord node) =>
   SequData (TD.FlowTopology node) ->
   (Map Idx.State (TD.FlowTopology node),
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
   (Ord node, Arith.Sum a) =>
   Map Idx.Section Idx.State ->
   Env.Complete node a a ->
   StateEnv.Complete node a a
envFromSequenceEnv secMap (Env.Complete scalar signal) =
   StateEnv.Complete
      (scalarEnvFromSequenceEnv secMap scalar)
      (signalEnvFromSequenceEnv secMap signal)

scalarEnvFromSequenceEnv ::
   (Ord node, Arith.Sum a) =>
   Map Idx.Section Idx.State ->
   Env.Scalar node a ->
   StateEnv.Scalar node a
scalarEnvFromSequenceEnv secMap (Env.Scalar _me _st se _sx sis sos) =
   StateEnv.Scalar
      (cumulateScalarMap
         (\(Idx.StEnergy (Idx.StorageEdge from to)) ->
            Idx.StEnergy $ Idx.StorageEdge
               (fmap (UMap.checkedLookup "cumulate StEnergyMap from" secMap) from)
               (fmap (UMap.checkedLookup "cumulate StEnergyMap to"   secMap) to))
         se)
      Map.empty
      (cumulateScalarMap
         (\(Idx.StInSum aug) ->
            Idx.StInSum $
            fmap (UMap.checkedLookup "cumulate StInSumMap" secMap) aug) sis)
      (cumulateScalarMap
         (\(Idx.StOutSum aug) ->
            Idx.StOutSum $
            fmap (UMap.checkedLookup "cumulate StOutSumMap" secMap) aug) sos)

cumulateScalarMap ::
   (Ord node, Ord (stateIdx node), Arith.Sum a) =>
   (secIdx node -> stateIdx node) ->
   Map (Idx.ForNode secIdx node) a ->
   Map (Idx.ForNode stateIdx node) a
cumulateScalarMap f =
   Map.mapKeysWith (~+)
      (\(Idx.ForNode aug node) -> Idx.ForNode (f aug) node)


signalEnvFromSequenceEnv ::
   (Ord node, Arith.Sum a) =>
   Map Idx.Section Idx.State ->
   Env.Signal node a ->
   StateEnv.Signal node a
signalEnvFromSequenceEnv secMap (Env.Signal e _p _n dt _x s) =
   StateEnv.Signal
      (cumulateSignalMap secMap e)
      Map.empty
      Map.empty
      (cumulateSignalMap secMap dt)
      Map.empty
      (cumulateSignalMap secMap s)

cumulateSignalMap ::
   (Ord (idx node), Arith.Sum a) =>
   Map Idx.Section Idx.State ->
   Map (Idx.InSection idx node) a ->
   Map (Idx.InState idx node) a
cumulateSignalMap secMap =
   Map.mapKeysWith (~+)
      (\(Idx.InSection sec idx) ->
         Idx.InState (UMap.checkedLookup "cumulateSignalMap" secMap sec) idx)


stateFlow ::
   (Ord node, Arith.Sum a) =>
   SequData (TD.FlowTopology node) ->
   Env.Complete node a a ->
   (Map Idx.State (TD.FlowTopology node),
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


insEdges ::
   Ord node =>
   [TD.FlowEdge Gr.EitherEdge (Idx.AugStateNode node)] ->
   StateFlowGraph node ->
   StateFlowGraph node
insEdges = Gr.insEdges . map (flip (,) ())

initSecNode :: node -> Idx.AugStateNode node
initSecNode = Idx.TimeNode (Idx.NoExit Idx.Init)

exitSecNode :: node -> Idx.AugStateNode node
exitSecNode = Idx.TimeNode Idx.Exit

stateGraph ::
   (Ord node, Show node) =>
   SequData (FlowTopology node) ->
   StateFlowGraph node
stateGraph sd =
   insEdges
      (map (TD.FlowEdge . TD.StorageEdge) $ Fold.fold $
       Map.mapWithKey (map . flip Idx.ForNode) $ fmap storageEdges $
       fmap (Map.mapMaybe id) tracks) $
   Gr.insNodes
      (concatMap (\n ->
          [(initSecNode n, TD.Storage $ Just TD.In),
           (exitSecNode n, TD.Storage $ Just TD.Out)]) $
       Map.keys tracks) $
   Fold.fold $
   Map.mapWithKey stateFromClassTopo sq
  where sq = fmap TD.classifyStorages $ fst $ stateMaps sd
        tracks = getStorageSequences sq
