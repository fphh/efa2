{-# LANGUAGE TypeOperators #-}

module Modules.Utility where

import qualified EFA.Graph.Topology.Index as TIdx
-- import qualified EFA.Example.Index as XIdx
import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.Record as EqRec
import EFA.Equation.Result (Result(..))
import EFA.Utility.Map (checkedLookup)
import qualified Modules.System as System
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Data (Data(..), Nil, (:>))
import qualified EFA.Signal.ConvertTable as CT
-- import qualified EFA.IO.TableParser as Table
import qualified EFA.IO.TableParserTypes as TPT

import qualified Data.Map as M

lookupAbsPower ::
  (Ord node, Show d, Show node,Num d,Fractional d) =>
  TIdx.InSection TIdx.Power node ->
  Maybe (EqEnv.Complete node b (EqRec.Absolute (Result d))) -> d
lookupAbsPower n = maybe (-1000) f
  where f env = case checkedLookup "Modules.Utility.lookupAbsPower"
                     (EqEnv.powerMap $ EqEnv.signal env) n of
                    EqRec.Absolute (Determined x) -> x
                    EqRec.Absolute (Undetermined) -> error $ "Modules.Utility.lookupAbsPower - not determined : " ++ show n



-- | Warning -- only works for one section in env
envToPowerRecord ::  EqEnv.Complete
                     System.Node
                     (EqRec.Absolute (Result (Data  Nil a)))
                     (EqRec.Absolute (Result (Data (v :> Nil) a))) ->
                     Sig.TSignal v a -> Integer -> Record.PowerRecord System.Node v a
envToPowerRecord env time sec =
  Record.Record time
    (M.map i $ M.mapKeys h $ M.filterWithKey p $ EqEnv.powerMap $ EqEnv.signal env)
  where p (TIdx.InSection (TIdx.Section section) (TIdx.Power (TIdx.StructureEdge _ _))) _ =
          toInteger section == sec
        h (TIdx.InSection (TIdx.Section _) (TIdx.Power (TIdx.StructureEdge n1 n2))) =
          TIdx.PPos (TIdx.StructureEdge n1 n2)

        i (EqRec.Absolute (Determined dat)) = Sig.TC dat
        i (EqRec.Absolute Undetermined) =
          error "Modules.Utility.envToPowerRecord - undetermined data"


-- | Warum verwenden wir hier niht checkedLookup -- Fehlermeldung niht klar genug, kein caller eingezogen
getEtas :: M.Map String (a -> a) -> [String] -> [a -> a]
getEtas etaFunc = map $
  \str -> maybe (error $ "getEtas :" ++ str ++ " not found") id (M.lookup str etaFunc)


getPowerSignals ::
  M.Map String (TPT.T Double) ->
  [String] ->
  [(Sig.TSignal [] Double, Sig.PSignal [] Double)]
getPowerSignals tabPower =
    map (f . CT.convertToSignal2D .
         flip (M.findWithDefault (error "getPowerSignals: signal not found")) tabPower)
  where f (x, [y]) = (x, y)
        f (_, []) = error "getTimes: no power data available"
