{-# LANGUAGE Rank2Types #-}

module EFA.Application.OneStorage where

import EFA.Graph.Topology (StateFlowGraph)
import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Graph.StateFlow.Environment as SFEnv

import EFA.Equation.Result (Result(..))


import qualified EFA.Application.Sweep as Sweep

import qualified Data.Map as Map; import Data.Map (Map)


data SocDrive a = NoDrive
                | ChargeDrive a
                | DischargeDrive a deriving (Show, Eq)


noforcing ::
  (Num v) => SocDrive v -> SFEnv.Complete node b (Result v) -> v
noforcing _ _ = 0


nocondition :: SFEnv.Complete node b (Result v) -> Bool
nocondition _ = True


-- type OptimalPower node = Map TIdx.State [XIdxState.Power node]
type OptimalPower node = Map TIdx.State [(node, node)]



optimalPower :: [(TIdx.State, [(node, node)])] -> OptimalPower node
optimalPower = Map.fromList
{-
  List.foldr f Map.empty 
  where f (s, lst) = 
          Map.insertWith' (++) s (map (uncurry (XIdxState.power s)) lst)
-}

type OptimalEtaWithEnv node v = Map TIdx.State (Map (node, node) (Map [v] (v, v)))

data OptimalEnvParams node v = OptimalEnvParams {
  etaMap :: Map String (v -> v),
  points :: Sweep.Points v,
  optimalPowers :: OptimalPower node,
  stateFlowGraph :: StateFlowGraph node }
