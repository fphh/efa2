
module EFA.Application.EtaSys where

import qualified EFA.Application.Index as XIdx
import qualified EFA.Application.IndexState as XIdxState

import qualified EFA.Application.Utility as AppUt

import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Graph.StateFlow.Environment as EqEnvState

import EFA.Equation.Result (Result(..))

import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr

import qualified Data.Set as Set
import qualified Data.Map as Map


import Control.Applicative (liftA2)

import Data.Traversable (sequenceA)
import Data.Maybe (mapMaybe)

import Data.Tuple.HT (fst3, thd3)


hasStructureEdge :: Set.Set a -> Bool
hasStructureEdge = not . Set.null


etaSys ::
  (Show a, Fractional a, Show node, Ord node) =>
  Flow.RangeGraph node ->
  EqEnv.Complete node b (Result a) -> Result a
etaSys (_, topo) env = liftA2 (/) (sumRes sinks) (sumRes sources)
  where m = Map.elems $
            Gr.nodeEdges $
            Gr.lefilter (TD.isStructureEdge . fst) $
            TD.dirFromFlowGraph topo

        sinks = concatMap (mapMaybe sinkEnergies . Set.toList . fst3) $ filter isActiveSink m
        sources = concatMap (mapMaybe sourceEnergies . Set.toList . thd3) $ filter isActiveSource m

        sumRes = fmap sum . sequenceA

        isActiveSink (ns, TD.AlwaysSink, _) = hasStructureEdge ns
        isActiveSink (ns, TD.Sink, _) = hasStructureEdge ns
        isActiveSink _ = False

        isActiveSource (_, TD.AlwaysSource, ns) = hasStructureEdge ns
        isActiveSource (_, TD.Source, ns) = hasStructureEdge ns
        isActiveSource _ = False

        sinkEnergies

          (TD.FlowEdge (TD.StructureEdge e)) =
            Just $ AppUt.lookupAbsEnergy "etaSys, sinkEnergies" env $ Idx.flip $
            Idx.liftInPart (Idx.Energy . TD.structureEdgeFromDirEdge) e
        sinkEnergies _ = Nothing

        sourceEnergies
          (TD.FlowEdge (TD.StructureEdge e)) =
            Just $ AppUt.lookupAbsEnergy "etaSys, sourceEnergies" env $
            Idx.liftInPart (Idx.Energy . TD.structureEdgeFromDirEdge) e

        sourceEnergies _ = Nothing

etaSysState ::
  (Show a, Fractional a, Show node, Ord node) =>
  TD.StateFlowGraph node ->
  EqEnvState.Complete node b (Result a) -> Result a
etaSysState topo env = liftA2 (/) (sumRes sinks) (sumRes sources)
  where m = Map.elems $
            Gr.nodeEdges $
            Gr.lefilter (TD.isStructureEdge . fst) $
            TD.dirFromFlowGraph topo

        sinks = concatMap (mapMaybe sinkEnergies . Set.toList . fst3) $ filter isActiveSink m
        sources = concatMap (mapMaybe sourceEnergies . Set.toList . thd3) $ filter isActiveSource m

        sumRes = fmap sum . sequenceA

        isActiveSink (ns, TD.AlwaysSink, _) = hasStructureEdge ns
        isActiveSink (ns, TD.Sink, _) = hasStructureEdge ns
        isActiveSink _ = False

        isActiveSource (_, TD.AlwaysSource, ns) = hasStructureEdge ns
        isActiveSource (_, TD.Source, ns) = hasStructureEdge ns
        isActiveSource _ = False

        sinkEnergies
          (TD.FlowEdge (TD.StructureEdge (Idx.InPart sec (Gr.DirEdge a b)))) =
            Just $ AppUt.lookupAbsEnergyState "etaSys, sinkEnergies" env (XIdxState.energy sec b a)
        sinkEnergies _ = Nothing

        sourceEnergies
          (TD.FlowEdge (TD.StructureEdge (Idx.InPart sec (Gr.DirEdge a b)))) =
            Just $ AppUt.lookupAbsEnergyState "etaSys, sourceEnergies" env (XIdxState.energy sec a b)
        sourceEnergies _ = Nothing


detEtaSys ::
  (Fractional v, Ord node, Show node, Show v) =>
  Flow.RangeGraph node -> EqEnv.Complete node a (Result v) -> v
detEtaSys topo = AppUt.checkDetermined "detEtaSys" . etaSys topo

detEtaSysState ::
  (Fractional v, Ord node, Show node, Show v) =>
  TD.StateFlowGraph node -> EqEnvState.Complete node a (Result v) -> v
detEtaSysState topo =
  AppUt.checkDetermined "detEtaSysState\n" . etaSysState topo


type Condition node a v = EqEnv.Complete node a (Result v) -> Bool
type ConditionState node a v = EqEnvState.Complete node a (Result v) -> Bool

type Forcing node a v = EqEnv.Complete node a (Result v) -> v
type ForcingState node a v = EqEnvState.Complete node a (Result v) -> v


objectiveFunction ::
  (Fractional v, Show v, Num v, Ord node, Show node) =>
  Condition node a v ->
  Forcing node a v ->
  Flow.RangeGraph node ->
  EqEnv.Complete node a (Result v) ->
  Maybe v
objectiveFunction cond forcing topo env =
  case cond env of
       True -> Just $ detEtaSys topo env + forcing env
       False -> Nothing


objectiveFunctionState ::
  (Fractional v, Show v, Num v, Ord node, Show node) =>
  ConditionState node a v ->
  ForcingState node a v ->
  TD.StateFlowGraph node ->
  EqEnvState.Complete node a (Result v) ->
  Maybe v
objectiveFunctionState cond forcing topo env =
  case cond env of
       True -> Just $ detEtaSysState topo env + forcing env
       False -> Nothing
