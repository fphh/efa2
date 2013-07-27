{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module EFA.Application.Optimisation where

import qualified EFA.Application.Absolute as EqGen
import qualified EFA.Application.Index as XIdx
import EFA.Application.Absolute ( (=.=) )

import qualified EFA.Signal.Data as Data
import EFA.Signal.Data (Data(..), Nil)

import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD


import qualified EFA.Equation.Environment as EqEnv

import qualified EFA.Equation.Arithmetic as EqArith

import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import Data.Map (Map)
import Data.Monoid((<>))




-- | TODO Functios below could ventually be moved to a module Application/Given


-- | Function to specifiy that an efficiency function in etaAssign is to be looked up with input power
etaOverPowerIn :: XIdx.Eta node -> XIdx.Power node
etaOverPowerIn =
   TIdx.liftInSection $ \(TIdx.Eta e) -> TIdx.Power $ TIdx.flip e

-- | Function to specifiy that an efficiency function in etaAssign is to be looked up with output power
etaOverPowerOut :: XIdx.Eta node -> XIdx.Power node
etaOverPowerOut =
   TIdx.liftInSection $ \(TIdx.Eta e) -> TIdx.Power e


type EtaAssignMap node =
        Map (XIdx.Eta node) (String, String, XIdx.Eta node -> XIdx.Power node)


-- | Generate given equations using efficiency curves or functions for a specified section
makeEtaFuncGiven ::
   (Fractional a, Ord a, Show a, EqArith.Sum a,
    Data.Apply c a ~ v, Eq v, Data.ZipWith c, Data.Storage c a, Node.C node) =>
   (TIdx.Section -> EtaAssignMap node) ->
   TIdx.Section ->
   Map String (a -> a) ->
   EqGen.EquationSystem node s x (Data c a)
makeEtaFuncGiven etaAssign sec etaFunc = Fold.fold $ Map.mapWithKey f (etaAssign sec)
  where f n (strP, strN, g) =
          EqGen.variable n =.= EqGen.liftF (Data.map ef) (EqGen.variable $ g n)
          where ef x = if x >= 0 then fpos x else fneg x
                fpos = maybe (err strP) id (Map.lookup strP etaFunc)
                fneg = maybe (err strN) (\h -> recip . h . negate)
                                        (Map.lookup strN etaFunc)
                err str x = error ("not defined: " ++ show str ++ " for " ++ show x)

-- | Takes all non-energy and non-power values from an env, removes values in section x and generate given equations
givenAverageWithoutSectionX ::(Eq v, EqArith.Sum v, Node.C node,
                Ord node,Eq a, EqArith.Sum a) =>
               TIdx.Section ->
               EqEnv.Complete node a v  ->
               EqGen.EquationSystem node s a v

givenAverageWithoutSectionX secToRemove (EqEnv.Complete scalar signal) =
   (EqGen.fromMap $ EqEnv.dtimeMap signal) <>
   (EqGen.fromMap $ Map.filterWithKey f $ EqEnv.etaMap signal) <>
   (EqGen.fromMap $ Map.filterWithKey f $ EqEnv.xMap signal) <>
   (EqGen.fromMap $ EqEnv.stXMap scalar) <>
   (EqGen.fromMap $ EqEnv.stInSumMap scalar) <>
   (EqGen.fromMap $ EqEnv.stOutSumMap scalar)
   where
     f :: TIdx.InSection idx node -> v -> Bool
     f (TIdx.InPart sec _) _ = sec /= secToRemove

givenForOptimisation :: (EqArith.Constant a,
                         Node.C node,
                         Fractional a,
                         Ord a,
                         Show a,
                         EqArith.Sum a,
                         Ord node) =>
   Flow.RangeGraph node ->
   EqEnv.Complete node (Data Nil a) (Data Nil a)  ->
   (TIdx.Section -> EtaAssignMap node) ->
   Map String (a -> a) ->
   TIdx.Section ->
   EqGen.EquationSystem node s (Data Nil a) (Data Nil a) ->
   EqGen.EquationSystem node s (Data Nil a) (Data Nil a) ->
   EqGen.EquationSystem node s (Data Nil a) (Data Nil a) ->
   EqGen.EquationSystem node s (Data Nil a) (Data Nil a)

givenForOptimisation seqTopology env etaAssign etaFunc sec commonGiven givenLoad givenDOF =
  commonGiven <>
  EqGen.fromGraph True (TD.dirFromFlowGraph (snd seqTopology)) <>
  makeEtaFuncGiven etaAssign sec etaFunc <>
  givenAverageWithoutSectionX sec env <>
  givenLoad <>
  givenDOF
