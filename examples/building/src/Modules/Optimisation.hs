{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Modules.Optimisation where

import qualified Modules.System as System
import Modules.System (Node(..))

import qualified EFA.Application.Optimisation as AppOpt
import qualified EFA.Application.AbsoluteState as EqGen
-- import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Utility as AppUt
import EFA.Application.AbsoluteState ((.=))

import qualified EFA.Equation.Arithmetic as EqArith
import EFA.Equation.Result (Result(..))

import qualified EFA.Flow.State.Index as XIdx

import qualified EFA.Graph.StateFlow.Environment as StateEnv
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Signal.Data as Data
import EFA.Signal.Data (Data(..), Nil)

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import Data.Map (Map)
import Data.Monoid (mconcat, (<>))
import Data.Foldable (foldMap)
import Data.Maybe (catMaybes, isJust)
--import Data.List (filter)


state0, state1, state2, state3 :: Idx.State
state0 :~ state1 :~ state2 :~ state3 :~ _ = Stream.enumFrom $ Idx.State 0


type Env a = StateEnv.Complete Node (Data Nil a) (Data Nil a)
--type EnvResultData a = StateEnv.Complete Node (Result (Data Nil a)) (Result (Data Nil a))
type EnvResult a = StateEnv.Complete Node (Result a) (Result a)

type EqSystemData a =
  forall s. EqGen.EquationSystem System.Node s (Data Nil a) (Data Nil a)

type EtaAssignMap node =
  Map (XIdx.Eta node) (String, String, XIdx.Eta node -> XIdx.Power node)

solve ::
  (Ord a, Fractional a, Show a, EqArith.Sum a, EqArith.Constant a) =>
  TD.StateFlowGraph Node ->
  (Idx.State -> EtaAssignMap Node) ->
  Map String (a -> a) ->
  Env a ->
  Idx.State ->
  a -> a -> a -> a -> EnvResult a
solve stateFlowGraph etaAssign etaFunc env state pLocal pRest pWater pGas =
  envGetData $ EqGen.solveSimple $
    AppOpt.givenForOptimisation
      stateFlowGraph
      env
      etaAssign
      etaFunc
      state
      commonGiven
      (givenSecLoad state (Data pLocal) (Data pRest))
      (givenSecDOF state (Data pWater) (Data pGas))

givenSecLoad :: (Eq a, EqArith.Sum a) =>
                Idx.State ->
                Data Nil a ->
                Data Nil a ->
                EqSystemData a
givenSecLoad state pLocal pRest =  mconcat $
   (XIdx.power state LocalRest LocalNetwork .= pLocal) :
   (XIdx.power state Rest Network .= pRest) :
   []

givenSecDOF :: (Eq a, EqArith.Sum a) =>
                Idx.State ->
               Data Nil a ->
               Data Nil a ->
               EqSystemData a
givenSecDOF state pWater pGas =  mconcat $
   (XIdx.power state Network Water .= pWater) :
   (XIdx.power state LocalNetwork Gas .= pGas) :
   []


--  @HT wir können leider keine Speicherenergien für den Stateflow definieren
commonGiven ::
  (EqArith.Sum a, Num a, Eq a) =>
  EqSystemData a
commonGiven =
   foldMap f idx
   where f st = (XIdx.dTime st .= Data 1)
                <> (XIdx.energy state0 Water Network
                     EqGen.=%%= XIdx.energy st Water Network)
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

--condition :: StateEnv.Complete Node b (Result Double) -> Bool
--condition env = True



condition :: (Show b) => StateEnv.Complete Node b (Result Double) -> Bool
condition env =
  all (>0) $ catMaybes $ takeWhile isJust $ cnlst ++ nlnlst
--  all (>0) $ catMaybes $ filter isJust $ cnlst ++ nlnlst
  where idx = [Idx.State 0 ..]
        coalNetworkFunc state =
          AppUt.lookupEnergyStateMaybe (XIdx.energy state Coal Network) env
        networkLocalNetworkFunc state =
          AppUt.lookupEnergyStateMaybe (XIdx.energy state Network LocalNetwork) env

        cnlst = map coalNetworkFunc idx
        nlnlst = map networkLocalNetworkFunc idx
