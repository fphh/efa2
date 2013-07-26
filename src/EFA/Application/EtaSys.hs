
module EFA.Application.EtaSys where

import qualified EFA.Application.Index as XIdx
import qualified EFA.Application.Utility as AppUt

import qualified EFA.Equation.Environment as EqEnv
import EFA.Equation.Result (Result(..))

import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr

import EFA.Utility.Map (checkedLookup)

import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Applicative (liftA2)

import Data.Traversable (sequenceA)
import Data.Maybe (mapMaybe)

import Data.Tuple.HT (fst3, thd3)


lookupAbsEnergy ::
  (Ord node, Show node, Show t) =>
  String ->
  EqEnv.Complete node b (Result t) ->
  XIdx.Energy node -> Result t
lookupAbsEnergy caller env n =
  checkedLookup caller (EqEnv.energyMap $ EqEnv.signal env) n


lookupAbsPower ::
  (Ord node, Show node, Show t) =>
  String ->
  EqEnv.Complete node b (Result t) ->
  XIdx.Power node -> Result t
lookupAbsPower caller env n =
  checkedLookup caller (EqEnv.powerMap $ EqEnv.signal env) n


lookupAbsEta ::
  (Ord node, Show node, Show t) =>
  String ->
  EqEnv.Complete node b (Result t) ->
  XIdx.Eta node -> Result t
lookupAbsEta caller env n =
  checkedLookup caller (EqEnv.etaMap $ EqEnv.signal env) n

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
          (TD.FlowEdge (TD.StructureEdge (Idx.InPart sec (Gr.DirEdge a b)))) =
            Just $ lookupAbsEnergy "etaSys, sinkEnergies" env (XIdx.energy sec b a)
        sinkEnergies _ = Nothing

        sourceEnergies
          (TD.FlowEdge (TD.StructureEdge (Idx.InPart sec (Gr.DirEdge a b)))) =
            Just $ lookupAbsEnergy "etaSys, sourceEnergies" env (XIdx.energy sec a b)
        sourceEnergies _ = Nothing



detEtaSys ::
  (Fractional c, Ord node, Show node, Show c) =>
  Flow.RangeGraph node -> EqEnv.Complete node b (Result c) -> c
detEtaSys topo = AppUt.checkDetermined "detEtaSys" . etaSys topo
