{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}


module EFA.Example.EtaSys where

import qualified EFA.Equation.Record as EqRec
import qualified EFA.Equation.Environment as EqEnv
import EFA.Equation.Result (Result(..))

import qualified EFA.Graph as Gr
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Graph.Flow as Flow
import EFA.Signal.Data (Data(..), Nil, (:>))


import qualified EFA.Example.Index as XIdx

import EFA.Utility.Map (checkedLookup)
import qualified EFA.Utility.Bifunctor as BF

import Data.Tuple.HT (fst3, thd3)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

import Control.Applicative (liftA2)

import Data.Traversable (sequenceA)

import Data.Maybe (mapMaybe)

import Data.Ord (comparing)

import Data.Eq.HT (equating)


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

{-
-- HH: Muss repariert werden!!!


newtype InBalance node a = InBalance (Map.Map node a) deriving (Show)

instance Functor (InBalance node) where
         fmap f (InBalance m) = InBalance (fmap f m)


newtype OutBalance node a = OutBalance (Map.Map node a) deriving (Show)

instance Functor (OutBalance node) where
         fmap f (OutBalance m) = OutBalance (fmap f m)

data InOutBalance node a =
  InOutBalance (InBalance node a) (OutBalance node a) deriving (Show)


instance Functor (InOutBalance node) where
         fmap f (InOutBalance ins outs) = InOutBalance (fmap f ins) (fmap f outs)


{-
storageBalance ::
  (Show node, Ord node, Show a) =>
  Flow.RangeGraph node ->
  EqEnv.Complete node b (EqRec.Absolute (Result a)) ->
  InOutBalance node (Result [a])
-}

storageBalance (_, topo) env = InOutBalance inBalance outBalance
  where structTopo = Gr.lefilter (TD.isStructureEdge . fst) $
                     TD.dirFromSequFlowGraph topo


        m = Map.elems $ Gr.nodeEdges structTopo

        inSt = concatMap (map Gr.to . Set.toList . fst3) $ filter isInSt m
        outSt = concatMap (map Gr.from . Set.toList . thd3) $ filter isOutSt m

        -- hier soll ein Map das groupBy und sortBy ersetzen!!!
        inStEs = map (concatMap inFunc) $ 
                 List.groupBy (equating getNode) $ 
                 List.sortBy (comparing getNode) inSt
        outStEs = map (concatMap outFunc) $
                  List.groupBy (equating getNode) $
                  List.sortBy (comparing getNode) outSt
        getNode (TIdx.ForNode _ x) = x

        inFunc x = map (g x . getNode) $ Gr.pre structTopo x
        outFunc x = map (g x . getNode) $ Gr.suc structTopo x

        g (TIdx.ForNode (TIdx.AfterSection sec) x) y =
          (x, lookupAbsEnergy "storageBalance" env (XIdx.energy sec x y))


        isInSt (ns, TD.Storage _, _) = hasStructureEdge ns
        isInSt _ = False

        isOutSt (_, TD.Storage _, ns) = hasStructureEdge ns
        isOutSt _ = False

        inBalance = InBalance $ Map.fromList $ map h inStEs
        outBalance = OutBalance $ Map.fromList $ map h outStEs
        h xs = (fst $ head xs, sequenceA $ map snd xs)



sumBalance ::
  (Num a) => 
  InOutBalance node (Result [a]) -> InOutBalance node (Result a)

sumBalance bal = fmap (fmap sum) bal



type Balance node a = Map.Map node a



bal ::
  (Show node, Ord node, Show a, Num a) =>
  Flow.RangeGraph node ->
  EqEnv.Complete node b (EqRec.Absolute (Result a)) ->
  Balance node (Result a)
bal topo env = Set.fold f Map.empty keys
  where InOutBalance (InBalance ins) (OutBalance outs) =
          sumBalance $ storageBalance topo env
        keys = Set.union (Map.keysSet ins) (Map.keysSet outs)
        lu k = maybe (Determined 0) id . Map.lookup k
        f k = Map.insert k (liftA2 (-) (lu k ins) (lu k outs))


{-
balance ::
  (Num d, Ord node, Show d, Show node) =>
  Flow.RangeGraph node ->
  EqEnv.Complete node b (EqRec.Absolute (Result (Data ([] :> Nil) d))) ->
  Map.Map node (Result d)
-}

balance ::
  (Num d, Ord node, Show d, Show node) =>
  Flow.RangeGraph node ->
  EqEnv.Complete node b (EqRec.Absolute (Result (Data ([] :> Nil) d))) ->
  Map.Map node (Result d)



balance topo env = bal topo (BF.second (fmap (fmap (\(Data x) -> sum x))) env)
-}
