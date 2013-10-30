{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Modules.Optimisation where

import qualified Modules.System as System

import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.Optimisation as AppOpt
import EFA.Application.Utility (checkDetermined)
import EFA.Application.Simulation (EtaAssignMap)

import qualified EFA.Flow.State.Absolute as EqSys
import qualified EFA.Flow.State.Quantity as StateFlow
import qualified EFA.Flow.State.Index as XIdx
import EFA.Flow.State.Absolute ((.=))

import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.Topology.Variable as TopoVar

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Result as Result
import EFA.Equation.Result (Result)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Signal.Data as Data
import EFA.Signal.Data (Data(Data), Nil)

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty
import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Monoid (mconcat, (<>))
import Data.Foldable (foldMap)



state0, state1, state2, state3 :: Idx.State
state0 :~ state1 :~ state2 :~ state3 :~ _ = Stream.enumFrom $ Idx.State 0


type EnvData a = StateFlow.Graph System.Node (Data Nil a) (Data Nil a)
type EnvResult a = StateFlow.Graph System.Node (Result a) (Result a)
type EnvResultData a = EnvResult (Data Nil a)

type EqSystemData a =
  forall s. EqSys.EquationSystemIgnore System.Node s (Data Nil a) (Data Nil a)

type Param2 = NonEmpty.T (NonEmpty.T Empty.T)

type Param2x2 = Sweep.Pair Param2 Param2

solve ::
  (Ord a, Show a, Arith.Constant a) =>
  EnvResultData a ->
  EtaAssignMap System.Node ->
  Map String (a -> a) ->
  Idx.State ->
  Param2x2 a -> EnvResult a
solve stateFlowGraph etaAssign etaFunc state (Sweep.Pair load dof) =
  envGetData $
    EqSys.solveOpts
      (EqSys.equalInOutSums EqSys.optionsDefault)
      (AppOpt.givenAverageWithoutState state stateFlowGraph) $
    (AppOpt.makeEtaFuncGiven state stateFlowGraph etaAssign etaFunc
      <> commonGiven
      <> givenSecLoad state (fmap Data load)
      <> givenSecDOF state (fmap Data dof))

givenSecLoad ::
   (Arith.Sum a, Eq a) =>
   Idx.State -> Param2 (Data Nil a) -> EqSystemData a
givenSecLoad state (NonEmpty.Cons pLocal (NonEmpty.Cons pRest Empty.Cons)) =
   mconcat $
   (XIdx.power state System.LocalRest System.LocalNetwork .= pLocal) :
   (XIdx.power state System.Rest System.Network .= pRest) :
   []

givenSecDOF ::
   (Arith.Sum a, Eq a) =>
   Idx.State -> Param2 (Data Nil a) -> EqSystemData a
givenSecDOF state (NonEmpty.Cons pWater (NonEmpty.Cons pGas Empty.Cons)) =
   mconcat $
   (XIdx.power state System.Network System.Water .= pWater) :
   (XIdx.power state System.LocalNetwork System.Gas .= pGas) :
   []


--  @HT wir können leider keine Speicherenergien für den Stateflow definieren
commonGiven ::
  (Arith.Constant a) =>
  EqSystemData a
commonGiven =
   foldMap f idx
   where f st = (XIdx.dTime st .= Data (Arith.fromInteger 1))
       --         <> (XIdx.energy state0 System.Water System.Network
       --              EqSys.=%%= XIdx.energy st System.Water System.Network)
         idx = [Idx.State 1 .. Idx.State 1] -- Noch sieben, weil wir acht Zustände haben im Zustandsflussgraphen!!!


--
--   mconcat $
--   (XIdx.dTime state0 .= Data 1) :
--   (XIdx.dTime state4 .= Data 1) :
   -- ((XIdx.stEnergy Idx.Init Idx.Exit Water) EqSys.=%%= (XIdx.energy state1 Water Network)) :

--   (XIdx.energy state0 Water Network EqSys.=%%= XIdx.energy state4 Water Network) :



--   (XIdx.storage Idx.initial Wasser .= Data 0) :
--   (XIdx.storage Idx.initial Batterie .= Data 0) :
--   (XIdx.energy state0 Wasser Network =%%= XIdx.energy state1 Wasser Network) :
--   (XIdx.energy state0 Batterie LocalNetwork =%%= XIdx.energy state1 LocalNetwork Batterie) :
--   []





-- | Unpack scalar result values in env from Data constructor
envGetData ::
  (Ord node) =>
  StateFlow.Graph node (Result (Data.Data va a)) (Result (Data.Data vv v)) ->
  StateFlow.Graph node (Result (Data.Apply va a)) (Result (Data.Apply vv v))
envGetData =
  StateFlow.mapGraph (fmap Data.getData) (fmap Data.getData)


{-
data SocDrive a = NoDrive
                | ChargeDrive a
                | DischargeDrive a deriving (Show, Eq)

-}

-----------------------------------------------------------------------------

condition :: EnvResult Double -> Bool
condition = conditionLax

conditionTrivial :: EnvResult Double -> Bool
conditionTrivial _env = True

conditionLax :: EnvResult Double -> Bool
conditionLax =
   Fold.all (all (>0)) .
   Map.mapWithKey
      (\state topo ->
         let name = "Optimisation.condition " ++ show state
         in  mapMaybe Result.toMaybe $
             TopoVar.checkedLookup name FlowTopo.lookupEnergy
                (TopoIdx.energy System.Coal System.Network) topo :
             TopoVar.checkedLookup name FlowTopo.lookupEnergy
                (TopoIdx.energy System.Network System.LocalNetwork) topo :
             []) .
   StateFlow.states

conditionStrict :: EnvResult Double -> Bool
conditionStrict =
   Fold.all (all (>0)) .
   Map.mapWithKey
      (\state topo ->
         let name = "Optimisation.condition " ++ show state
         in  map (checkDetermined name) $
             TopoVar.checkedLookup name FlowTopo.lookupEnergy
                (TopoIdx.energy System.Coal System.Network) topo :
             TopoVar.checkedLookup name FlowTopo.lookupEnergy
                (TopoIdx.energy System.Network System.LocalNetwork) topo :
             []) .
   StateFlow.states
