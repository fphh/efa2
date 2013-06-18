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
import qualified EFA.Signal.Data as Data


import qualified EFA.Example.Index as XIdx

import EFA.Utility.Map (checkedLookup)
import qualified EFA.Utility.Bifunctor as BF

import Data.Tuple.HT (fst3, thd3)

import qualified EFA.Signal.Signal as Sig

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

import Control.Applicative (liftA2)

import Data.Traversable (sequenceA)

import Data.Ord (comparing)

import Debug.Trace


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


hasStructureEdge ::
  (Ord node, Eq node) =>
  S.Set (TD.FlowEdge Gr.DirEdge (TIdx.BndNode node)) -> Bool
hasStructureEdge = not . S.null


etaSys ::
  (Show a, Num a, Fractional a, Show node, Ord node) =>
  Flow.RangeGraph node ->
  EqEnv.Complete node b (EqRec.Absolute (Result a)) -> Result a
etaSys (_, topo) env = liftA2 (/) (sumRes sinks) (sumRes sources)
  where m = M.elems $ 
            Gr.nodeEdges $
            Gr.lefilter (TD.isStructureEdge . fst) $
            TD.dirFromSequFlowGraph topo

        sinks = concatMap (map sinkEnergies . S.toList . fst3) $ filter isActiveSink m
        sources = concatMap (map sourceEnergies . S.toList . thd3) $ filter isActiveSource m

        sumRes = fmap sum . sequenceA

        isActiveSink (ns, TD.AlwaysSink, _) = hasStructureEdge ns
        isActiveSink (ns, TD.Sink, _) = hasStructureEdge ns
        isActiveSink _ = False

        isActiveSource (_, TD.AlwaysSource, ns) = hasStructureEdge ns
        isActiveSource (_, TD.Source, ns) = hasStructureEdge ns
        isActiveSource _ = False
 
        sinkEnergies
          (TD.FlowEdge (TD.StructureEdge (TIdx.InSection sec (Gr.DirEdge a b)))) =
            lookupAbsEnergy "etaSys, sinkEnergies" env (XIdx.energy sec b a)

        sourceEnergies 
          (TD.FlowEdge (TD.StructureEdge (TIdx.InSection sec (Gr.DirEdge a b)))) =
            lookupAbsEnergy "etaSys" env (XIdx.energy sec a b)

equalBy :: (Eq b) => (a -> b) -> a -> a -> Bool
equalBy f x y = f x == f y


newtype InBalance node a = InBalance (M.Map node a) deriving (Show)

instance Functor (InBalance node) where
         fmap f (InBalance m) = InBalance (fmap f m)


newtype OutBalance node a = OutBalance (M.Map node a) deriving (Show)

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


        m = M.elems $ Gr.nodeEdges structTopo

        inSt = concatMap (map Gr.to . S.toList . fst3) $ filter isInSt m
        outSt = concatMap (map Gr.from . S.toList . thd3) $ filter isOutSt m

        inStEs = map (concatMap inFunc) $ 
                 L.groupBy (equalBy getNode) $ 
                 L.sortBy (comparing getNode) inSt
        outStEs = map (concatMap outFunc) $
                  L.groupBy (equalBy getNode) $
                  L.sortBy (comparing getNode) outSt
        getNode (TIdx.BndNode _ x) = x

        inFunc x = map (g x . getNode) $ Gr.pre structTopo x
        outFunc x = map (g x . getNode) $ Gr.suc structTopo x

        g (TIdx.BndNode (TIdx.AfterSection sec) x) y =
          (x, lookupAbsEnergy "storageBalance" env (XIdx.energy sec x y))


        isInSt (ns, TD.Storage _, _) = hasStructureEdge ns
        isInSt _ = False

        isOutSt (_, TD.Storage _, ns) = hasStructureEdge ns
        isOutSt _ = False

        inBalance = InBalance $ M.fromList $ map h inStEs
        outBalance = OutBalance $ M.fromList $ map h outStEs
        h xs = (fst $ head xs, sequenceA $ map snd xs)



sumBalance ::
  (Num a) => 
  InOutBalance node (Result [a]) -> InOutBalance node (Result a)

sumBalance bal = fmap (fmap sum) bal



type Balance node a = M.Map node a



bal ::
  (Show node, Ord node, Show a, Num a) =>
  Flow.RangeGraph node ->
  EqEnv.Complete node b (EqRec.Absolute (Result a)) ->
  Balance node (Result a)
bal topo env = S.fold f M.empty keys
  where InOutBalance (InBalance ins) (OutBalance outs) =
          sumBalance $ storageBalance topo env
        keys = S.union (M.keysSet ins) (M.keysSet outs)
        lu k = maybe (Determined 0) id . M.lookup k
        f k = M.insert k (liftA2 (-) (lu k ins) (lu k outs))


balance ::
  (Num d, Ord node, Show d, Show node) =>
  Flow.RangeGraph node ->
  EqEnv.Complete node b (EqRec.Absolute (Result (Data ([] :> Nil) d))) ->
  M.Map node (Result d)
balance topo env = bal topo (BF.second (fmap (fmap (\(Data x) -> sum x))) env)
