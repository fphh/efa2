{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module EFA.Application.Optimisation where

--import qualified EFA.Application.Optimisation.Balance as Forcing
import qualified EFA.Application.Optimisation.Params as Params
import qualified EFA.Application.Optimisation.Sweep as Sweep

import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.State as State

import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Flow.Topology.Variable as TopoVar
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Flow.Storage.Variable as StorageVar
import qualified EFA.Flow.Storage as Storage

import EFA.Signal.Data (Data, Nil)

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
   StateQty.Graph node (Result a) (Result v) ->
   StateQty.Graph node (Result a) (Result v)
givenAverageWithoutState focus given =
   StateQty.mapGraphWithVar
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


initialEnvWithoutState ::
  (Ord node, Arith.Constant b,
   Sweep.SweepClass sweep vec b,
   Sweep.SweepMap sweep vec b b) =>
  Params.Optimisation node list sweep vec b ->
  Maybe Idx.State ->
  StateQty.Graph node a v ->
  StateQty.Graph node (Result (sweep vec b)) (Result (sweep vec b))
initialEnvWithoutState optParams state =
  let -- one = One.one params
      one = Sweep.fromRational (Params.sweepLength optParams) Arith.one
      mkSweep x = Determined $ Sweep.map (const (Arith.fromRational x)) one
  in StateQty.mapGraphWithVar
       (\(Idx.ForStorage var _) _a ->
         case var of
            StorageVar.X _ -> mkSweep 0.5
            _ -> Undetermined)
       (\(Idx.InPart st var) _v ->
         case var of
            TopoVar.Eta _ -> mkSweep 0.5
            TopoVar.DTime _ -> Determined one
            TopoVar.X _ ->
               let f s = if s == st then Undetermined else x
                   x = mkSweep 0.2
               in  maybe x f state
            _ -> Undetermined)

{-
initialEnvWithoutState ::
   (Ord node, Arith.Constant d) =>
   Maybe Idx.State ->
   StateQty.Graph node (Result a) (Result v) ->
   StateQty.Graph node (Result (Data Nil d)) (Result (Data Nil d))
initialEnvWithoutState state =
   StateQty.mapGraphWithVar
      (\(Idx.ForStorage var _) _a ->
         fmap Data $
         case var of
            StorageVar.X _ -> Determined $ Arith.fromRational 0.5
            _ -> Undetermined)
      (\(Idx.InPart st var) _v ->
         fmap Data $
         case var of
            TopoVar.Eta _ -> Determined $ Arith.fromRational 0.5
            TopoVar.DTime _ -> Determined $ Arith.fromRational 1
            TopoVar.X _ ->
               let f s = if s == st then Undetermined else x
                   x = Determined $ Arith.fromRational 0.2
               in  maybe x f state
            _ -> Undetermined)
-}


eraseXAndEtaFromState ::
   (Ord node) =>
   Idx.State ->
   StateQty.Graph node a (Result b) ->
   StateQty.Graph node a (Result b)
eraseXAndEtaFromState state =
  StateQty.mapGraphWithVar
      (\(Idx.ForStorage _var _) a -> a)
      (\(Idx.InPart st var) v ->
         case var of
            TopoVar.X _ -> if st == state then Undetermined else v
            TopoVar.Eta _ -> if st == state then Undetermined else v
            _ -> v)

eraseEnergies ::
   (Ord node) =>
   StateQty.Graph node (Result (Data Nil d)) (Result (Data Nil d)) ->
   StateQty.Graph node (Result (Data Nil d)) (Result (Data Nil d))
eraseEnergies =
  StateQty.mapGraphWithVar
      (\(Idx.ForStorage _var _) a -> a)
      (\(Idx.InPart _st var) v ->
         case var of
            TopoVar.Energy _ -> Undetermined
            _ -> v)

initialEnv ::
  (Ord node, Arith.Constant b,
   Sweep.SweepClass sweep vec b,
   Sweep.SweepMap sweep vec b b) =>
  Params.Optimisation node list sweep vec b ->
  StateQty.Graph node a v ->
  StateQty.Graph node (Result (sweep vec b)) (Result (sweep vec b))
initialEnv optParams = initialEnvWithoutState optParams Nothing


storageEdgeXFactors ::
  (Fractional b, Arith.Constant b,
   Sweep.SweepClass sweep vec b,
   Sweep.SweepMap sweep vec b b) =>
  Params.Optimisation node list sweep vec b ->
  Integer ->
  Integer ->
  StateQty.Graph node (Result (sweep vec b)) v ->
  StateQty.Graph node (Result (sweep vec b)) v
storageEdgeXFactors optParams nout nin g = g { State.storages = tt }
   where one = Sweep.fromRational (Params.sweepLength optParams) Arith.one
         tt = Map.map f $ State.storages g
         f gr = gr { Storage.edges = Map.map func $ Storage.edges gr }
         func x = x { StateQty.carryXOut = xout, StateQty.carryXIn = xin }
         xout = Determined $ Sweep.map (/Arith.fromInteger nout) one
         xin = Determined $ Sweep.map (/Arith.fromInteger nin) one

