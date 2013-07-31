{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Modules.Optimisation where

import qualified Modules.System as System
import Modules.System (Node(..))

-- import qualified EFA.Application.Absolute as EqGen
import qualified EFA.Application.AbsoluteState as EqGen

--import EFA.Equation.System((=%%=))
--import EFA.Graph.StateFlow.EquationSystem((=%%=))


import qualified EFA.Application.IndexState as XIdx
--import qualified EFA.Application.Utility as EqUt
--import qualified EFA.Application.EtaSys as ES
--import EFA.Application.Absolute ( (.=)) --(=.=) )
import EFA.Application.AbsoluteState ( (.=)) --(=.=) )


--import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Graph.StateFlow.Environment as EqEnv
import qualified EFA.Equation.Arithmetic as EqArith
import EFA.Equation.Result (Result(..))

import qualified EFA.Graph.Topology.Index as TIdx
--import qualified EFA.Graph.Flow as Flow
--import qualified EFA.Graph.Topology.Node as TDNode
import qualified EFA.Graph.Topology as TD

--import qualified EFA.Signal.SequenceData as SD
--import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Signal as Sig
--import EFA.Signal.Typ(UT,Typ)
--import EFA.Signal.Signal(TC(..))
import qualified EFA.Signal.Data as Data
--import qualified EFA.Signal.Base as Base
--import qualified EFA.Signal.Vector as SV

import EFA.Signal.Data (Data(..),
                        Nil)
                        --(:>))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

--import qualified Data.Map as Map ;
import Data.Map (Map)
-- import qualified Data.Vector as V
--import qualified Data.Foldable as Fold
import Data.Monoid (mconcat) --, (<>))

--import EFA.Application.Utility (envGetData) --makeEdges, select,
--import EFA.Application.Optimisation (etaOverPowerIn, etaOverPowerOut)
import qualified EFA.Application.Optimisation as AppOpt

state0, state1 :: TIdx.State
state0 :~ state1 :~ _ = Stream.enumFrom $ TIdx.State 0

type Env a = EqEnv.Complete Node (Data Nil a) (Data Nil a)
type EnvResultData a = EqEnv.Complete Node (Result (Data Nil a)) (Result (Data Nil a))
type EnvResult a = EqEnv.Complete Node (Result a) (Result a)

type EqSystemData a =  (forall s. EqGen.EquationSystem System.Node s (Data Nil a) (Data Nil a))

type EtaAssignMap node = Map (XIdx.Eta node) (String, String, XIdx.Eta node -> XIdx.Power node)

solve ::
  (Ord a, Fractional a, Show a, EqArith.Sum a, EqArith.Constant a) =>
  TD.StateFlowGraph Node ->
  Env a ->
  TIdx.State ->
  (TIdx.State -> EtaAssignMap Node)->
  Map String (a -> a) ->
  a ->
  a ->
  a ->
  a ->
  a ->
  a ->
  EnvResult a
solve stateFlowGraph env state etaAssign etaFunc pHouse pNetload pWater pBattery pSun pCoal =
  envGetData $ EqGen.solveSimple $
    AppOpt.givenForOptimisation stateFlowGraph env etaAssign etaFunc state
      commonGiven
      (givenSecLoad state (Data pHouse) (Data pNetload) (Data pSun) (Data pCoal))
      (givenSecDOF state (Data pWater) (Data pBattery))

givenSecLoad :: (Eq a, EqArith.Sum a) =>
                TIdx.State ->
                Data Nil a ->
                Data Nil a ->
                Data Nil a ->
                Data Nil a ->
                EqSystemData a
givenSecLoad state pHouse pNetload pSun pCoal =  mconcat $
   (XIdx.power state Hausnetz Verteiler .= pHouse) :
   (XIdx.power state Netzlast Netz .= pNetload) :
   (XIdx.power state Sonne Verteiler .= pSun) :
   (XIdx.power state Kohle Netz .= pCoal) :
   []

givenSecDOF :: (Eq a, EqArith.Sum a) =>
                TIdx.State ->
               Data Nil a ->
               Data Nil a ->
               EqSystemData a
givenSecDOF state pWater pBattery =  mconcat $
   (XIdx.power state Netz Wasser .= pWater) :
   (XIdx.power state Verteiler Batterie .= pBattery) :
   []


--  @HT wir können leider keine Speicherenergien für den Stateflow definieren
commonGiven ::
  (EqArith.Sum a, Num a, Eq a) =>
  EqSystemData a
commonGiven =
   mconcat $
   (XIdx.dTime state0 .= Data 1) :
   (XIdx.dTime state1 .= Data 1) :
   -- (XIdx.energy state0 Wasser Netz EqGen.=%%= XIdx.energy state1 Wasser Netz) :
   -- (XIdx.energy state0 Batterie Verteiler EqGen.=%%= XIdx.energy state1 Batterie Verteiler) :



--   (XIdx.storage TIdx.initial Wasser .= Data 0) :
--   (XIdx.storage TIdx.initial Batterie .= Data 0) :
--   (XIdx.energy state0 Wasser Netz =%%= XIdx.energy state1 Wasser Netz) :
--   (XIdx.energy state0 Batterie Verteiler =%%= XIdx.energy state1 Verteiler Batterie) :
   []





-- | Unpack scalar result values in env from Data constructor
envGetData ::
  (Ord node) =>
  EqEnv.Complete node (Result (Data.Data va a)) (Result (Data.Data vv v)) ->
  EqEnv.Complete node (Result (Data.Apply va a)) (Result (Data.Apply vv v))
envGetData =
  EqEnv.completeFMap (fmap Data.getData) (fmap Data.getData)


data SocDrive a = NoDrive
                | ChargeDrive a
                | DischargeDrive a deriving (Show, Eq)


forcing :: SocDrive Double -> EqEnv.Complete Node b (Result Double) -> Double
forcing socDrive env = 0
{-
case socDrive of
       NoDrive -> 0
       ChargeDrive soc -> soc * eCharge
       DischargeDrive soc -> soc * negate eDischarge
  where eCharge    = lookupDetEnergy (XIdx.energy sec0 Water Network) env
        eDischarge = lookupDetEnergy (XIdx.energy sec1 Water Network) env
-}
-----------------------------------------------------------------------------

condition :: EqEnv.Complete Node b (Result Double) -> Bool
condition env = True


{-
condition :: EqEnv.Complete Node b (Result Double) -> Bool
condition env = all (>0) [eCoal0, eCoal1, eTrans0, eTrans1]
  where eCoal0     = lookupDetEnergy (XIdx.energy sec0 Coal Network) env
        eCoal1     = lookupDetEnergy (XIdx.energy sec1 Coal Network) env
        eTrans0    = lookupDetEnergy (XIdx.energy sec0 Network LocalNetwork) env
        eTrans1    = lookupDetEnergy (XIdx.energy sec1 Network LocalNetwork) env
-}




