{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Modules.Optimisation where

import qualified Modules.System as System

import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.Optimisation as AppOpt
import qualified EFA.Application.AbsoluteState as EqGen
-- import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Utility as AppUt
import EFA.Application.Simulation (EtaAssignMap)
import EFA.Application.AbsoluteState ((.=))

import qualified EFA.Equation.Arithmetic as EqArith
import EFA.Equation.Result (Result)

import qualified EFA.Flow.State.Index as XIdx

import qualified EFA.Graph.StateFlow.Environment as StateEnv
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Signal.Data as Data
import EFA.Signal.Data (Data(Data), Nil)

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty
import Data.Map (Map)
import Data.Monoid (mconcat, (<>))
import Data.Foldable (foldMap)
import Data.Maybe (catMaybes, isJust)
--import Data.List (filter)


state0, state1, state2, state3 :: Idx.State
state0 :~ state1 :~ state2 :~ state3 :~ _ = Stream.enumFrom $ Idx.State 0


type EnvData a = StateEnv.Complete System.Node (Data Nil a) (Data Nil a)
type EnvResult a = StateEnv.Complete System.Node (Result a) (Result a)
type EnvResultData a = EnvResult (Data Nil a)

type EqSystemData a =
  forall s. EqGen.EquationSystem System.Node s (Data Nil a) (Data Nil a)

type Param2 = NonEmpty.T (NonEmpty.T Empty.T)

type Param2x2 = Sweep.Pair Param2 Param2

solve ::
  (Ord a, Fractional a, Show a, EqArith.Constant a) =>
  Topo.StateFlowGraph System.Node ->
  EtaAssignMap System.Node ->
  Map String (a -> a) ->
  EnvData a ->
  Idx.State ->
  Param2x2 a -> EnvResult a
solve stateFlowGraph etaAssign etaFunc env state (Sweep.Pair load dof) =
  envGetData $ EqGen.solve stateFlowGraph $
    AppOpt.givenForOptimisation
      env
      etaAssign
      etaFunc
      state
      (commonGiven
        <> givenSecLoad state (fmap Data load)
        <> givenSecDOF state (fmap Data dof))

givenSecLoad ::
   (EqArith.Sum a, Eq a) =>
   Idx.State -> Param2 (Data Nil a) -> EqSystemData a
givenSecLoad state (NonEmpty.Cons pLocal (NonEmpty.Cons pRest Empty.Cons)) =
   mconcat $
   (XIdx.power state System.LocalRest System.LocalNetwork .= pLocal) :
   (XIdx.power state System.Rest System.Network .= pRest) :
   []

givenSecDOF ::
   (EqArith.Sum a, Eq a) =>
   Idx.State -> Param2 (Data Nil a) -> EqSystemData a
givenSecDOF state (NonEmpty.Cons pWater (NonEmpty.Cons pGas Empty.Cons)) =
   mconcat $
   (XIdx.power state System.Network System.Water .= pWater) :
   (XIdx.power state System.LocalNetwork System.Gas .= pGas) :
   []


--  @HT wir können leider keine Speicherenergien für den Stateflow definieren
commonGiven ::
  (EqArith.Sum a, Num a, Eq a) =>
  EqSystemData a
commonGiven =
   foldMap f idx
   where f st = (XIdx.dTime st .= Data 1)
                <> (XIdx.energy state0 System.Water System.Network
                     EqGen.=%%= XIdx.energy st System.Water System.Network)
         idx = take 20 [Idx.State 1 ..]
--
--   mconcat $
--   (XIdx.dTime state0 .= Data 1) :
--   (XIdx.dTime state4 .= Data 1) :
   -- ((XIdx.stEnergy Idx.Init Idx.Exit Water) EqGen.=%%= (XIdx.energy state1 Water Network)) :

--   (XIdx.energy state0 Water Network EqGen.=%%= XIdx.energy state4 Water Network) :



--   (XIdx.storage Idx.initial Wasser .= Data 0) :
--   (XIdx.storage Idx.initial Batterie .= Data 0) :
--   (XIdx.energy state0 Wasser Network =%%= XIdx.energy state1 Wasser Network) :
--   (XIdx.energy state0 Batterie LocalNetwork =%%= XIdx.energy state1 LocalNetwork Batterie) :
--   []





-- | Unpack scalar result values in env from Data constructor
envGetData ::
  (Ord node) =>
  StateEnv.Complete node (Result (Data.Data va a)) (Result (Data.Data vv v)) ->
  StateEnv.Complete node (Result (Data.Apply va a)) (Result (Data.Apply vv v))
envGetData =
  StateEnv.completeFMap (fmap Data.getData) (fmap Data.getData)


{-
data SocDrive a = NoDrive
                | ChargeDrive a
                | DischargeDrive a deriving (Show, Eq)

-}

--forcing :: One.SocDrive Double -> StateEnv.Complete Node b (Result Double) -> Double
--forcing socDrive env = 0
{-
case socDrive of
       NoDrive -> 0
       ChargeDrive soc -> soc * eCharge
       DischargeDrive soc -> soc * negate eDischarge
  where eCharge    = lookupDetEnergy (XIdx.energy sec0 Water Network) env
        eDischarge = lookupDetEnergy (XIdx.energy sec1 Water Network) env
-}
-----------------------------------------------------------------------------

--condition :: EnvResult Double -> Bool
--condition env = True



condition :: EnvResult Double -> Bool
condition env =
  all (>0) $ catMaybes $ takeWhile isJust $ cnlst ++ nlnlst
--  all (>0) $ catMaybes $ filter isJust $ cnlst ++ nlnlst
  where idx = [Idx.State 0 ..]
        coalNetworkFunc state =
          AppUt.lookupEnergyStateMaybe
            (XIdx.energy state System.Coal System.Network) env
        networkLocalNetworkFunc state =
          AppUt.lookupEnergyStateMaybe
            (XIdx.energy state System.Network System.LocalNetwork) env

        cnlst = map coalNetworkFunc idx
        nlnlst = map networkLocalNetworkFunc idx
