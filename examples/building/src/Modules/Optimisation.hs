{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Modules.Optimisation where

import qualified Modules.System as System
import Modules.System (Node(..))

import qualified EFA.Application.Absolute as EqGen
--import EFA.Equation.System((=%%=))

--import qualified EFA.Application.Index as XIdx
--import qualified EFA.Application.Utility as EqUt
--import qualified EFA.Application.EtaSys as ES
--import EFA.Application.Absolute ( (.=)) --(=.=) )

import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Graph.StateFlow.Environment as EqEnvState
import qualified EFA.Graph.StateFlow.EquationSystem as EqGenState ; import EFA.Graph.StateFlow.EquationSystem ((.=))-- (=.=))
import qualified EFA.Equation.Verify as Verify



import qualified EFA.Equation.Arithmetic as EqArith
import EFA.Equation.Result (Result(..))

import qualified EFA.Graph.Topology.Index as TIdx
-import qualified EFA.Graph.Flow as Flow
--import qualified EFA.Graph.Topology.Node as TDNode
import qualified EFA.Graph.Topology as TD
--import qualified EFA.Graph as Graph
import qualified EFA.Equation.Record as EqRecord
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

--import qualified Data.Map as Map ; import Data.Map (Map)
-- import qualified Data.Vector as V
--import qualified Data.Foldable as Fold
import Data.Monoid (mconcat) --, (<>))

--import EFA.Application.Utility (envGetData) --makeEdges, select,
--import EFA.Application.Optimisation (etaOverPowerIn, etaOverPowerOut)
--import qualified EFA.Application.Optimisation as AppOpt

sec0, sec1 :: TIdx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ TIdx.Section 0

state0, state1 :: TIdx.State
state0 :~ state1 :~ _ = Stream.enumFrom $ TIdx.State 0

type Env a = EqEnv.Complete Node (Data Nil a) (Data Nil a)
type EnvResultData a = EqEnv.Complete Node (Result (Data Nil a)) (Result (Data Nil a))
type EnvResult a = EqEnv.Complete Node (Result a) (Result a)

type EqSystemData a =  (forall s. EqGen.EquationSystem System.Node s (Data Nil a) (Data Nil a))

type EqSystemStateData a =  (forall s. EqGenState.EquationSystem Verify.Ignore EqRecord.Absolute System.Node s (Data Nil a) (Data Nil a))

{-
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
-}
{-solve2 ::
  (Ord a, Fractional a, Show a, EqArith.Sum a, EqArith.Constant a) =>
  Flow.RangeGraph Node ->
  Env a ->
  TIdx.Section ->
  Map String (a -> a) ->
  a ->
  a ->
  a ->
  a ->
  EnvResult a -}


{-solve2 :: (Fractional a, Ord a, Show a, EqArith.Constant a) =>
                (Map TIdx.State (Sig.SignalIdx, Sig.SignalIdx),
                 Graph.Graph
                 (TIdx.PartNode (TIdx.Exit (TIdx.Init TIdx.State)) Node)
                 (TD.FlowEdge Graph.EitherEdge)
                 (TD.NodeType (Maybe TD.StoreDir))
                 ())
                -> EqEnvState.Complete Node (Data Nil a) (Data Nil a)
                -> TIdx.State
                -> Map [Char] (a -> a)
                -> a
                -> a
                -> a
                -> a
                 -> EqEnvState.Complete Node (Result a) (Result a)
--                -> EqEnvState.Complete Node (EqRecord.Absolute (Result (Data Nil a))) (EqRecord.Absolute (Result (Data Nil a)))-}
solve2 stateFlowGraph env state etaFunc pHouse pNetload pWater pBattery = stateEnvGetData $
  EqGenState.solveSimple $
    --AppOpt.givenForOptimisation stateFlowGraph env etaAssign etaFunc state
--      commonGiven
      (givenStateLoad state (Data pHouse) (Data pNetload))
--      (givenStateDOF state (Data pWater) (Data pBattery))

givenStateLoad :: (Eq a, EqArith.Sum a) =>
                TIdx.State ->
                Data Nil a ->
                Data Nil a ->
                EqSystemStateData a
givenStateLoad state pHouse pNetload =  mconcat $
   (TIdx.Record TIdx.Absolute (TIdx.InPart state (TIdx.Power(TIdx.StructureEdge Hausnetz Verteiler))) .= pHouse) :
   (TIdx.Record TIdx.Absolute (TIdx.InPart state (TIdx.Power(TIdx.StructureEdge Netzlast Netz))) .= pNetload) :
   []

givenStateDOF :: (Eq a, EqArith.Sum a) =>
                TIdx.State ->
               Data Nil a ->
               Data Nil a ->
               EqSystemStateData a
givenStateDOF state pWater pBattery =  mconcat $
   (TIdx.Record TIdx.Absolute (TIdx.InPart state (TIdx.Power (TIdx.StructureEdge Netz Wasser))) .= pWater) :
   (TIdx.Record TIdx.Absolute (TIdx.InPart state (TIdx.Power (TIdx.StructureEdge Verteiler Batterie))) .= pBattery) :
   []

{-
commonGiven ::
  (EqArith.Sum a, Num a, Eq a) =>
  EqSystemData a
commonGiven =
   mconcat $
   (XIdx.dTime state0 .= Data 1) :
   (XIdx.dTime state1 .= Data 1) :
   (XIdx.storage TIdx.initial Wasser .= Data 0) :
   (XIdx.energy state0 Wasser Netz =%%= XIdx.energy state1 Wasser Netz) :
   []


etaAssign ::
  TIdx.State ->
  Map (XIdx.Eta Node) (String, String, XIdx.Eta Node -> XIdx.Power Node)
etaAssign state = Map.fromList $
  (XIdx.eta state Wasser Netz, ( "storage", "storage", etaOverPowerOut)) :
  (XIdx.eta state Netz Wasser, ( "storage", "storage", etaOverPowerIn)) :

  (XIdx.eta state Kohle Netz, ( "coal", "coal", etaOverPowerOut)) :
  (XIdx.eta state Netz Kohle, ( "coal", "coal", etaOverPowerIn)) :

 -- (XIdx.eta state Gas Netz, ( "gas", "gas", etaOverPowerOut)) :
 -- (XIdx.eta state Netz Gas, ( "gas", "gas", etaOverPowerIn)) :

  (XIdx.eta state Netz Netzlast, ( "last", "last", etaOverPowerOut)) :
  (XIdx.eta state Netzlast Netz, ( "last", "last", etaOverPowerIn)) :

  []

-}


-- | Unpack scalar result values in env from Data constructor
stateEnvGetData ::
  (Ord node) =>
  EqEnvState.Complete node (EqRecord.Absolute (Result (Data.Data va a))) (EqRecord.Absolute(Result (Data.Data vv v))) ->
  EqEnvState.Complete node (Result (Data.Apply va a)) (Result (Data.Apply vv v))
stateEnvGetData =
  EqEnvState.completeFMap (fmap Data.getData . EqRecord.unAbsolute ) (fmap Data.getData . EqRecord.unAbsolute)
