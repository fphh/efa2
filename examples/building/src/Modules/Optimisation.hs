{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Modules.Optimisation where

import qualified Modules.System as System
import Modules.System (Node(..))

import qualified EFA.Application.Absolute as EqGen
import EFA.Equation.System((=%%=))

import qualified EFA.Application.Index as XIdx
--import qualified EFA.Application.Utility as EqUt
--import qualified EFA.Application.EtaSys as ES
import EFA.Application.Absolute ( (.=)) --(=.=) )



import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.Arithmetic as EqArith
import EFA.Equation.Result (Result(..))

import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Graph.Flow as Flow
--import qualified EFA.Graph.Topology.Node as TDNode
--import qualified EFA.Graph.Topology as TD

--import qualified EFA.Signal.SequenceData as SD
--import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Signal as Sig
--import EFA.Signal.Typ(UT,Typ)
--import EFA.Signal.Signal(TC(..))
--import qualified EFA.Signal.Data as Data
--import qualified EFA.Signal.Base as Base
--import qualified EFA.Signal.Vector as SV

import EFA.Signal.Data (Data(..),
                        Nil)
                        --(:>))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified Data.Map as Map ; import Data.Map (Map)
-- import qualified Data.Vector as V
--import qualified Data.Foldable as Fold
import Data.Monoid (mconcat) --, (<>))

import EFA.Application.Utility (envGetData) --makeEdges, select,
import EFA.Application.Optimisation (etaOverPowerIn, etaOverPowerOut)
import qualified EFA.Application.Optimisation as AppOpt

sec0, sec1 :: TIdx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ TIdx.Section 0

type Env a = EqEnv.Complete Node (Data Nil a) (Data Nil a)
type EnvResultData a = EqEnv.Complete Node (Result (Data Nil a)) (Result (Data Nil a))
type EnvResult a = EqEnv.Complete Node (Result a) (Result a)

type EqSystemData a =  (forall s. EqGen.EquationSystem System.Node s (Data Nil a) (Data Nil a))

solve ::
  (Ord a, Fractional a, Show a, EqArith.Sum a, EqArith.Constant a) =>
  Flow.RangeGraph Node ->
  Env a ->
  TIdx.Section ->
  Map String (a -> a) ->
  a ->
  a ->
  a ->
  a ->
  EnvResult a
solve seqTopology env sec etaFunc pHouse pNetload pWater pBattery =
  envGetData $ EqGen.solveSimple $
    AppOpt.givenForOptimisation seqTopology env etaAssign etaFunc sec
      commonGiven
      (givenSecLoad sec (Data pHouse) (Data pNetload))
      (givenSecDOF sec (Data pWater) (Data pBattery))

givenSecLoad :: (Eq a, EqArith.Sum a) =>
                TIdx.Section ->
                Data Nil a ->
                Data Nil a ->
                EqSystemData a
givenSecLoad sec pHouse pNetload =  mconcat $
   (XIdx.power sec Hausnetz Verteiler .= pHouse) :
   (XIdx.power sec Netzlast Netz .= pNetload) :
   []

givenSecDOF :: (Eq a, EqArith.Sum a) =>
                TIdx.Section ->
               Data Nil a ->
               Data Nil a ->
               EqSystemData a
givenSecDOF sec pWater pBattery =  mconcat $
   (XIdx.power sec Netz Wasser .= pWater) :
   (XIdx.power sec Verteiler Batterie .= pBattery) :
   []


commonGiven ::
  (EqArith.Sum a, Num a, Eq a) =>
  EqSystemData a
commonGiven =
   mconcat $
   (XIdx.dTime sec0 .= Data 1) :
   (XIdx.dTime sec1 .= Data 1) :
   (XIdx.storage TIdx.initial Wasser .= Data 0) :
   (XIdx.energy sec0 Wasser Netz =%%= XIdx.energy sec1 Wasser Netz) :
   []


etaAssign ::
  TIdx.Section ->
  Map (XIdx.Eta Node) (String, String, XIdx.Eta Node -> XIdx.Power Node)
etaAssign sec = Map.fromList $
  (XIdx.eta sec Wasser Netz, ( "storage", "storage", etaOverPowerOut)) :
  (XIdx.eta sec Netz Wasser, ( "storage", "storage", etaOverPowerIn)) :

  (XIdx.eta sec Kohle Netz, ( "coal", "coal", etaOverPowerOut)) :
  (XIdx.eta sec Netz Kohle, ( "coal", "coal", etaOverPowerIn)) :

  (XIdx.eta sec Gas Netz, ( "gas", "gas", etaOverPowerOut)) :
  (XIdx.eta sec Netz Gas, ( "gas", "gas", etaOverPowerIn)) :

  (XIdx.eta sec Netz Netzlast, ( "last", "last", etaOverPowerOut)) :
  (XIdx.eta sec Netzlast Netz, ( "last", "last", etaOverPowerIn)) :

  []



