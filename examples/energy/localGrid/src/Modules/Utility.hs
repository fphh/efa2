{-# LANGUAGE TypeOperators #-}

module Modules.Utility where

import qualified Modules.System as System

import EFA.Application.Utility (checkDetermined)

import qualified EFA.Flow.Sequence.Quantity as SeqFlow

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import EFA.Equation.Result (Result)

import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Data (Data, Nil, (:>))

import qualified EFA.Utility.Map as MapU

import qualified Data.Map as Map ; import Data.Map (Map)

-- | Warning -- only works for one section in env
envToPowerRecord ::
   SeqFlow.Graph System.Node
      (Result (Data Nil a))
      (Result (Data (v :> Nil) a)) ->
   Sig.TSignal v a -> Idx.Section ->
   Record.PowerRecord System.Node v a
envToPowerRecord gr time sec =
   Record.Record time $
   Map.unionsWith (error "envToPowerRecord: duplicate edges") $
   Map.elems $
   Map.mapWithKey
      (\e flow ->
         let se = Topo.structureEdgeFromDirEdge e
             toSig = Sig.TC . checkDetermined "envToPowerRecord"
         in  Map.fromList $
                (Idx.PPos se, toSig $ SeqFlow.flowPowerOut flow) :
                (Idx.PPos $ Idx.flip se, toSig $ SeqFlow.flowPowerIn flow) :
                []) $
   Graph.edgeLabels $ SeqFlow.dirFromFlowGraph $ snd $ snd $
   flip (MapU.checkedLookup "envToPowerRecord") sec $
   SeqFlow.sequence gr


-- | no checkedLookup because this would require Show (a -> a)
getEtas :: Map String (a -> a) -> [String] -> [a -> a]
getEtas etaFunc = map $
  \str -> Map.findWithDefault (error $ "getEtas :" ++ str ++ " not found") str etaFunc
