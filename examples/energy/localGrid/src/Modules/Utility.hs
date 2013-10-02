{-# LANGUAGE TypeOperators #-}

module Modules.Utility where

import qualified Modules.System as System

import EFA.Application.Utility (checkDetermined)

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.Environment as EqEnv
import EFA.Equation.Result (Result)

import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Data (Data, Nil, (:>))

import qualified Data.Map as Map ; import Data.Map (Map)

-- | Warning -- only works for one section in env
envToPowerRecord :: EqEnv.Complete
                      System.Node
                      (Result (Data  Nil a))
                      (Result (Data (v :> Nil) a)) ->
                    Sig.TSignal v a -> Idx.Section ->
                    Record.PowerRecord System.Node v a
envToPowerRecord env time sec =
  Record.Record time
    (Map.map (Sig.TC . checkDetermined "envToPowerRecord") $
     Map.mapKeys h $ Map.filterWithKey p $ EqEnv.powerMap $ EqEnv.signal env)
  where p (Idx.InPart section (Idx.Power _)) _  =  section == sec
        h (Idx.InPart _ (Idx.Power edge))  =  Idx.PPos edge


-- | no checkedLookup because this would require Show (a -> a)
getEtas :: Map String (a -> a) -> [String] -> [a -> a]
getEtas etaFunc = map $
  \str -> Map.findWithDefault (error $ "getEtas :" ++ str ++ " not found") str etaFunc
