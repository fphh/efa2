{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module EFA.Application.Optimisation where

import EFA.Application.Simulation (EtaAssignMap, checkFoundPair, absEtaFunction)

import qualified EFA.Flow.Topology.Variable as TopoVar
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Flow.State.Quantity as StateFlow
import qualified EFA.Flow.State.Index as StateIdx
import qualified EFA.Flow.State.Absolute as EqSysState
import qualified EFA.Flow.SequenceState.Index as Idx
import EFA.Flow.State.Absolute ((=.=))

import qualified EFA.Flow.Storage.Variable as StorageVar

import qualified EFA.Signal.Data as Data
import EFA.Signal.Data (Data(Data), Nil)

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified EFA.Graph.Topology.Node as Node

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import Data.Map (Map)


-- | TODO Functions below could eventually be moved to a module Application/Given


{- |
Function to specify that an efficiency function in etaAssign
is to be looked up with input power
-}
etaOverPowerIn ::
   Idx.InPart part TopoIdx.Eta node -> Idx.InPart part TopoIdx.Power node
etaOverPowerIn =
   Idx.liftInPart $ \(TopoIdx.Eta e) -> TopoIdx.Power $ TopoIdx.flip e

{- |
Function to specify that an efficiency function in etaAssign
is to be looked up with output power
-}
etaOverPowerOut ::
   Idx.InPart part TopoIdx.Eta node -> Idx.InPart part TopoIdx.Power node
etaOverPowerOut =
   Idx.liftInPart $ \(TopoIdx.Eta e) -> TopoIdx.Power e


givenAverageWithoutState ::
   (Arith.Sum a, Arith.Sum v, Node.C node) =>
   Idx.State ->
   StateFlow.Graph node (Result a) (Result v) ->
   StateFlow.Graph node (Result a) (Result v)
givenAverageWithoutState stateToRemove =
   StateFlow.mapGraphWithVar
      (\(Idx.ForStorage var _) a ->
         case var of
            StorageVar.X _ -> a
            _ -> Undetermined)
      (\(Idx.InPart state var) v ->
         if state == stateToRemove
           then Undetermined
           else
              case var of
                 TopoVar.DTime _ -> v
                 TopoVar.Eta _ -> v
                 TopoVar.X _ -> v
                 _ -> Undetermined)


makeEtaFuncGiven ::
   (Node.C node, Show a, Ord a, Arith.Constant a,
    Data.ZipWith c, Data.Storage c a) =>
   Idx.State ->
   StateFlow.Graph node (Result a0) (Result v0) ->
   EtaAssignMap node ->
   Map String (a -> a) ->
   EqSysState.EquationSystemIgnore node s x (Data c a)
makeEtaFuncGiven state flowGraph etaAssign etaFunc =
   Fold.fold $
   Map.mapWithKey
      (\se (strP, strN) ->
         EqSysState.variable (etaFromEdge flowGraph state se)
         =.=
         EqSysState.liftF
            (Data.map (absEtaFunction strP strN etaFunc))
            (EqSysState.variable (Idx.InPart state $ TopoIdx.Power se)))
      etaAssign

etaFromEdge ::
   Node.C node =>
   StateFlow.Graph node a0 v0 ->
   Idx.State -> TopoIdx.Edge node -> StateIdx.Eta node
etaFromEdge flowGraph state se =
   let etaF = Idx.InPart state $ TopoIdx.Eta se
       etaB = Idx.InPart state $ TopoIdx.Eta $ TopoIdx.flip se
   in  checkFoundPair etaF etaB
          (StateFlow.lookup etaF flowGraph,
           StateFlow.lookup etaB flowGraph)


initialEnv ::
   (Ord node, Arith.Constant d) =>
   StateFlow.Graph node (Result a) (Result v)  ->
   StateFlow.Graph node (Result (Data Nil d)) (Result (Data Nil d))
initialEnv =
   StateFlow.mapGraphWithVar
      (\(Idx.ForStorage var _) _a ->
         fmap Data $
         case var of
            StorageVar.X _ -> Determined $ Arith.fromRational 0.5
            _ -> Undetermined)
      (\(Idx.InPart _state var) _v ->
         fmap Data $
         case var of
            TopoVar.Eta _ -> Determined $ Arith.fromRational 0.5
            TopoVar.DTime _ -> Determined $ Arith.fromRational 1
            TopoVar.X _ -> Determined $ Arith.fromRational 0.5
            _ -> Undetermined)
