
module EFA.Application.EtaSys where

import qualified EFA.Application.Index as XIdx

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Environment as EqEnv
import EFA.Equation.Result (Result(..))

import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr

import EFA.Utility.Map (checkedLookup)

import qualified Data.Set as Set
import qualified Data.Map as Map ; import Data.Map (Map)

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
  (Show a, Num a, Fractional a, Show node, Ord node) =>
  Flow.RangeGraph node ->
  EqEnv.Complete node b (Result a) -> Result a
etaSys (_, topo) env = liftA2 (/) (sumRes sinks) (sumRes sources)
  where m = Map.elems $
            Gr.nodeEdges $
            Gr.lefilter (TD.isStructureEdge . fst) $
            TD.dirFromSequFlowGraph topo

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
          (TD.FlowEdge (TD.StructureEdge (Idx.InSection sec (Gr.DirEdge a b)))) =
            Just $ lookupAbsEnergy "etaSys, sinkEnergies" env (XIdx.energy sec b a)
        sinkEnergies _ = Nothing

        sourceEnergies
          (TD.FlowEdge (TD.StructureEdge (Idx.InSection sec (Gr.DirEdge a b)))) =
            Just $ lookupAbsEnergy "etaSys, sourceEnergies" env (XIdx.energy sec a b)
        sourceEnergies _ = Nothing


type Balance node a = Map node a


{-
cf. Graph.Flow.getStorageSequences, Equation.System.getStorageSequences
-}
storageBalanceRelative ::
   (Ord node, Arith.Sum a) =>
   EqEnv.Complete node a v ->
   Map node (Map Idx.AugmentedSection a)
storageBalanceRelative (EqEnv.Complete env _) =
   Map.unionWith
      (Map.unionWith (error "storage cannot be In and Out at the same time"))
      (sequences (\(Idx.StInSum sec) -> Idx.augmentSection sec) $
       EqEnv.stInSumMap env)
      (sequences (\(Idx.StOutSum sec) -> Idx.augmentSection sec) $
       fmap Arith.negate $ EqEnv.stOutSumMap env)

storageBalanceAbsolute ::
   (Ord node, Arith.Sum a) =>
   EqEnv.Complete node a v ->
   Map node (Map Idx.Boundary a)
storageBalanceAbsolute (EqEnv.Complete env _) =
   sequences (\(Idx.Storage bnd) -> bnd) $ EqEnv.storageMap env

sequences ::
   (Ord node, Ord sec) =>
   (idx node -> sec) ->
   Map (Idx.ForNode idx node) a -> Map node (Map sec a)
sequences sec env =
   Map.unionsWith (Map.unionWith (error "duplicate section for node")) $
   map
      (\(Idx.ForNode idx node, a) ->
         Map.singleton node $ Map.singleton (sec idx) a) $
   Map.toList env
