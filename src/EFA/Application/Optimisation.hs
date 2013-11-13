{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module EFA.Application.Optimisation where

import qualified EFA.Flow.State.Quantity as StateFlow
import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Flow.Topology.Variable as TopoVar
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Flow.Storage.Variable as StorageVar

import EFA.Signal.Data (Data(Data), Nil)

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Result as Result
import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified EFA.Graph.Topology.Node as Node

import qualified Data.Map as Map
import Data.Map (Map)


-- | TODO Functions below could eventually be moved to a module Application/Given


{- |
Function to specify that an efficiency function in etaAssign
is to be looked up with input power
-}
{-# DEPRECATED etaOverPowerIn, etaOverPowerOut "not needed, adapt to EFA.Application.Simulation.EtaAssignMap style" #-}
etaOverPowerIn ::
   Idx.InPart part TopoIdx.Eta node -> Idx.InPart part TopoIdx.Power node
etaOverPowerIn =
   Idx.liftInPart $
      \(TopoIdx.Eta (TopoIdx.Position from to)) ->
         TopoIdx.Power $ TopoIdx.Position to from

{- |
Function to specify that an efficiency function in etaAssign
is to be looked up with output power
-}
etaOverPowerOut ::
   Idx.InPart part TopoIdx.Eta node -> Idx.InPart part TopoIdx.Power node
etaOverPowerOut =
   Idx.liftInPart $ \(TopoIdx.Eta pos) -> TopoIdx.Power pos


givenAverageWithoutState ::
   (Arith.Sum a, Arith.Sum v, Node.C node) =>
   Idx.State ->
   Map (TopoIdx.Power node) v ->
   StateFlow.Graph node (Result a) (Result v) ->
   StateFlow.Graph node (Result a) (Result v)
givenAverageWithoutState focus given =
   StateFlow.mapGraphWithVar
      (\(Idx.ForStorage var _) a ->
         case var of
            StorageVar.X _ -> a
            _ -> Undetermined)
      (\(Idx.InPart state var) v ->
         if state == focus
           then
              case var of
                 TopoVar.Power idx -> Result.fromMaybe $ Map.lookup idx given
                 _ -> Undetermined
           else
              case var of
                 TopoVar.DTime _ -> v
                 TopoVar.Eta _ -> v
                 TopoVar.X _ -> v
                 _ -> Undetermined)

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
