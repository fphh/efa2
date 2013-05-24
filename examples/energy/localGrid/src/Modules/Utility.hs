module Modules.Utility where

import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.Record as EqRec
import EFA.Equation.Result (Result(..))
import EFA.Utility.Map (checkedLookup)
import qualified EFA.Signal.SequenceData as SD


lookupAbsEnergy :: (Ord node, Show d, Show node) =>
                   TIdx.InSection TIdx.Energy node -> 
                   EqEnv.Complete node b (EqRec.Absolute (Result d)) ->                   
                   d
lookupAbsEnergy n env = case checkedLookup (EqEnv.energyMap $ EqEnv.signal env) n of
                  EqRec.Absolute (Determined x) -> x
                  EqRec.Absolute (Undetermined) -> error $ "not determined : " ++ show n 



lookupAbsPower :: (Ord node, Show d, Show node,Num d,Fractional d) =>
                   TIdx.InSection TIdx.Power node -> 
                   Maybe (EqEnv.Complete node b (EqRec.Absolute (Result d))) ->
                   d
lookupAbsPower n = maybe (-0.333) f
  where f env= case checkedLookup (EqEnv.powerMap $ EqEnv.signal env) n of
                    EqRec.Absolute (Determined x) -> x
                    EqRec.Absolute (Undetermined) -> error $ "not determined : " ++ show n 



{-

lookupAbsEta :: (Ord node, Show d, Show node) =>
                   TIdx.InSection TIdx.Power node -> 
                   Maybe (EqEnv.Complete node b (EqRec.Absolute (Result d))) ->                   
                   d
lookupAbsEta n env = case checkedLookup (EqEnv.powerMap $ EqEnv.signal env) n of
                  EqRec.Absolute (Determined x) -> x
                  EqRec.Absolute (Undetermined) -> error $ "not determined : " ++ show  n
-}

select :: [topo] -> [Int] -> SD.SequData topo
select ts = SD.fromList . map (ts !!)
