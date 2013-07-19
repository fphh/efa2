{-# LANGUAGE TypeOperators #-}

module Modules.Utility where

import qualified Modules.System as System

import qualified EFA.Graph.Topology.Index as TIdx

import qualified EFA.Equation.Environment as EqEnv
import EFA.Equation.Result (Result(..))

import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.ConvertTable as CT
import EFA.Signal.Data (Data(..), Nil, (:>))

import qualified EFA.IO.TableParserTypes as TPT

import EFA.Utility.Map (checkedLookup)

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Map as Map ; import Data.Map (Map)


lookupAbsPower ::
  (Ord node, Show d, Show node,Num d,Fractional d) =>
  TIdx.InSection TIdx.Power node ->
  Maybe (EqEnv.Complete node b (Result d)) -> d
lookupAbsPower n = maybe (-1000) f
  where f env = case checkedLookup "Modules.Utility.lookupAbsPower"
                     (EqEnv.powerMap $ EqEnv.signal env) n of
                    Determined x -> x
                    Undetermined -> error $ "Modules.Utility.lookupAbsPower - not determined : " ++ show n



-- | Warning -- only works for one section in env
envToPowerRecord :: EqEnv.Complete
                      System.Node
                      (Result (Data  Nil a))
                      (Result (Data (v :> Nil) a)) ->
                    Sig.TSignal v a -> TIdx.Section ->
                    Record.PowerRecord System.Node v a
envToPowerRecord env time sec =
  Record.Record time
    (Map.map i $ Map.mapKeys h $ Map.filterWithKey p $ EqEnv.powerMap $ EqEnv.signal env)
  where p (TIdx.InSection section (TIdx.Power (TIdx.StructureEdge _ _))) _ =
          section == sec
        h (TIdx.InSection (TIdx.Section _) (TIdx.Power (TIdx.StructureEdge n1 n2))) =
          TIdx.PPos (TIdx.StructureEdge n1 n2)

        i (Determined dat) = Sig.TC dat
        i Undetermined =
          error "Modules.Utility.envToPowerRecord - undetermined data"


-- | Warum verwenden wir hier niht checkedLookup -- Fehlermeldung nicht klar genug, kein caller eingezogen
getEtas :: Map String (a -> a) -> [String] -> [a -> a]
getEtas etaFunc = map $
  \str -> Map.findWithDefault (error $ "getEtas :" ++ str ++ " not found") str etaFunc


getPowerSignals ::
  Map String (TPT.T Double) ->
  [String] ->
  [(Sig.TSignal [] Double, Sig.PSignal [] Double)]
getPowerSignals tabPower =
    map (f . CT.convertToSignal2D .
         flip (Map.findWithDefault (error "getPowerSignals: signal not found")) tabPower)
  where f (x, NonEmpty.Cons y []) = (x, y)
