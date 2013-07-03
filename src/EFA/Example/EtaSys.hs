{-# LANGUAGE TypeOperators #-}


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

import qualified Data.Set as Set
import qualified Data.Map as Map ; import Data.Map (Map)

import Control.Applicative (liftA2)

import qualified Data.Foldable as Fold
import Data.Traversable (sequenceA)
import Data.Maybe (mapMaybe)

import Data.Tuple.HT (mapPair, fst3, thd3)


lookupAbsEnergy ::
  (Ord node, Show node, Show t) =>
  String ->
  EqEnv.Complete node b (EqRec.Absolute (Result t)) ->
  XIdx.Energy node -> Result t
lookupAbsEnergy caller env n = EqRec.unAbsolute $
  checkedLookup caller (EqEnv.energyMap $ EqEnv.signal env) n


lookupAbsPower ::
  (Ord node, Show node, Show t) =>
  String ->
  EqEnv.Complete node b (EqRec.Absolute (Result t)) ->
  XIdx.Power node -> Result t
lookupAbsPower caller env n = EqRec.unAbsolute $
  checkedLookup caller (EqEnv.powerMap $ EqEnv.signal env) n


lookupAbsEta ::
  (Ord node, Show node, Show t) =>
  String ->
  EqEnv.Complete node b (EqRec.Absolute (Result t)) ->
  XIdx.Eta node -> Result t
lookupAbsEta caller env n = EqRec.unAbsolute $
  checkedLookup caller (EqEnv.etaMap $ EqEnv.signal env) n

hasStructureEdge :: Set.Set a -> Bool
hasStructureEdge = not . Set.null


etaSys ::
  (Show a, Num a, Fractional a, Show node, Ord node) =>
  Flow.RangeGraph node ->
  EqEnv.Complete node b (EqRec.Absolute (Result a)) -> Result a
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
          (TD.FlowEdge (TD.StructureEdge (TIdx.InSection sec (Gr.DirEdge a b)))) =
            Just $ lookupAbsEnergy "etaSys, sinkEnergies" env (XIdx.energy sec b a)
        sinkEnergies _ = Nothing

        sourceEnergies
          (TD.FlowEdge (TD.StructureEdge (TIdx.InSection sec (Gr.DirEdge a b)))) =
            Just $ lookupAbsEnergy "etaSys, sourceEnergies" env (XIdx.energy sec a b)
        sourceEnergies _ = Nothing


newtype InBalance node a = InBalance (Map node a) deriving (Show)

instance Functor (InBalance node) where
   fmap f (InBalance m) = InBalance (fmap f m)


newtype OutBalance node a = OutBalance (Map node a) deriving (Show)

instance Functor (OutBalance node) where
   fmap f (OutBalance m) = OutBalance (fmap f m)

data InOutBalance node inb outb =
   InOutBalance (InBalance node inb) (OutBalance node outb) deriving (Show)

{-
instance Functor (InOutBalance node) where
   fmap f (InOutBalance ins outs) = InOutBalance (fmap f ins) (fmap f outs)
-}


{-
siehe Graph.Flow, Graph.Draw
-}

storageBalance ::
   (Show node, Ord node, Show a) =>
   Flow.RangeGraph node ->
   EqEnv.Complete node a v ->
   InOutBalance node (Map TIdx.InitOrSection a) (Map TIdx.SectionOrExit a)

storageBalance (_, g) (EqEnv.Complete env _) =
   uncurry InOutBalance $
   mapPair
      (InBalance . checkedMapUnions,
       OutBalance . checkedMapUnions) $
   unzip $
   map
      (\view ->
         case view of
            TD.ViewNodeIn (TIdx.TimeNode sec node) ->
               (Map.singleton node $ Map.singleton sec $
                checkedLookup "storageBalance stOutSum" (EqEnv.stOutSumMap env) $
                XIdx.stOutSum sec node,
                Map.empty)
            TD.ViewNodeOut (TIdx.TimeNode sec node) ->
               (Map.empty,
                Map.singleton node $ Map.singleton sec $
                checkedLookup "storageBalance stInSum" (EqEnv.stInSumMap env) $
                XIdx.stInSum sec node)) $
   mapMaybe TD.viewNodeDir $
   Map.toList $
   Map.mapMaybe TD.maybeStorage $
   Gr.nodeLabels g

checkedMapUnions ::
   (Ord node, Ord sec) =>
   [Map node (Map sec a)] -> Map node (Map sec a)
checkedMapUnions =
   Map.unionsWith (Map.unionWith (error "duplicate section for node"))

sumBalance ::
   (Num inb, Num outb) =>
   InOutBalance node (Map isec inb) (Map osec outb) ->
   InOutBalance node inb outb

sumBalance (InOutBalance ins outs) =
   InOutBalance (fmap Fold.sum ins) (fmap Fold.sum outs)

diffBalance ::
   (Ord node, Num a) =>
   InOutBalance node a a -> Balance node a
diffBalance (InOutBalance (InBalance ins) (OutBalance outs)) =
   Map.intersectionWith (-) ins outs



type Balance node a = Map node a


balance ::
   (Show node, Ord node, Show a, Num a) =>
   Flow.RangeGraph node ->
   EqEnv.Complete node a v ->
   Balance node a
balance topo =
   diffBalance . sumBalance . storageBalance topo
