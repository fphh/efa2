{-# LANGUAGE TypeOperators #-}

module Modules.Utility where

import qualified Modules.System as System

import EFA.Application.Utility (checkDetermined)

import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Record as RecSeq

import qualified EFA.Graph.Topology.Index as Idx

import EFA.Equation.Result (Result)

import qualified EFA.Signal.Record as Record
import EFA.Signal.Data (Data, Nil, (:>))

import qualified EFA.Utility.Map as MapU

import qualified Data.Map as Map ; import Data.Map (Map)


-- | Warning -- only works for one section in env
envToPowerRecord ::
   Idx.Section ->
   SeqFlow.Graph System.Node
      (Result (Data Nil a))
      (Result (Data (v :> Nil) a)) ->
   Record.PowerRecord System.Node v a
envToPowerRecord sec =
   snd . flip (MapU.checkedLookup "envToPowerRecord") sec .
   RecSeq.flowGraphToPowerRecords .
   SeqFlow.mapGraph
      (checkDetermined "envToPowerRecord")
      (checkDetermined "envToPowerRecord")


-- | no checkedLookup because this would require Show (a -> a)
getEtas :: Map String (a -> a) -> [String] -> [a -> a]
getEtas etaFunc = map $
  \str -> Map.findWithDefault (error $ "getEtas :" ++ str ++ " not found") str etaFunc
