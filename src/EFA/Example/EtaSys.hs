

module EFA.Example.EtaSys where

import qualified EFA.Equation.Record as EqRec
import qualified EFA.Equation.Environment as EqEnv
import EFA.Equation.Result (Result(..))

import qualified EFA.Graph as Gr
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Graph.Flow as Flow

import qualified EFA.Example.Index as XIdx

import EFA.Utility.Map (checkedLookup)

import Data.Tuple.HT (fst3, thd3)

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

import Control.Applicative (liftA2)

import Data.Foldable as F


lookupAbsEnergy ::
  (Ord node, Show node, Show t) =>
  String ->
  EqEnv.Complete node b (EqRec.Absolute (Result t)) ->
  TIdx.InSection TIdx.Energy node -> Result t
lookupAbsEnergy caller env n = EqRec.unAbsolute $
  checkedLookup caller (EqEnv.energyMap $ EqEnv.signal env) n


lookupAbsPower ::
  (Ord node, Show node, Show t) =>
  String ->
  EqEnv.Complete node b (EqRec.Absolute (Result t)) ->
  TIdx.InSection TIdx.Power node -> Result t
lookupAbsPower caller env n = EqRec.unAbsolute $
  checkedLookup caller (EqEnv.powerMap $ EqEnv.signal env) n


lookupAbsEta ::
  (Ord node, Show node, Show t) =>
  String ->
  EqEnv.Complete node b (EqRec.Absolute (Result t)) ->
  TIdx.InSection TIdx.Eta node -> Result t
lookupAbsEta caller env n = EqRec.unAbsolute $
  checkedLookup caller (EqEnv.etaMap $ EqEnv.signal env) n



etaSys ::
  (Show a, Num a, Fractional a, Show node, Ord node) =>
  Flow.RangeGraph node ->
  EqEnv.Complete node b (EqRec.Absolute (Result a)) -> Result a
etaSys (_, topo) env = liftA2 (/) (sumRes sinks) (sumRes sources)
  where m = M.elems $ Gr.nodeEdges topo
        sinks = map (S.foldl sinkEnergies zero . fst3) $ filter isActiveSink m
        sources = map (S.foldl sourceEnergies zero . thd3) $ filter isActiveSource m

        zero = Determined 0
        add = liftA2 (+)
        sumRes = L.foldl' add zero

        isActiveSink (ns, TD.AlwaysSink, _) = p ns
        isActiveSink (ns, TD.Sink, _) = p ns
        isActiveSink _ = False

        isActiveSource (_, TD.AlwaysSource, ns) = p ns
        isActiveSource (_, TD.Source, ns) = p ns
        isActiveSource _ = False
 
        p = (> 0) . 
            S.size .
            S.filter
              (\(TD.FlowEdge (TD.StructureEdge (TIdx.InSection _ e))) -> TD.isActive e)

        sinkEnergies acc 
          (TD.FlowEdge (TD.StructureEdge (TIdx.InSection sec 
                       (Gr.EDirEdge (Gr.DirEdge a b))))) =
            add acc (lookupAbsEnergy "etaSys" env (XIdx.energy sec b a))

        sourceEnergies acc 
          (TD.FlowEdge (TD.StructureEdge (TIdx.InSection sec 
                       (Gr.EDirEdge (Gr.DirEdge a b))))) =
            add acc (lookupAbsEnergy "etaSys" env (XIdx.energy sec a b))

