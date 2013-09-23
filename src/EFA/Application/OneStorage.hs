module EFA.Application.OneStorage where

import EFA.Graph.Topology (StateFlowGraph)
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.StateFlow.Environment as StateEnv

import EFA.Equation.Result (Result)


import qualified EFA.Application.Sweep as Sweep

import qualified Data.Map as Map; import Data.Map (Map)


data SocDrive a = NoDrive
                | ChargeDrive a
                | DischargeDrive a deriving (Show, Eq)


noforcing ::
  (Num v) => SocDrive v -> StateEnv.Complete node b (Result v) -> v
noforcing _ _ = 0


nocondition :: StateEnv.Complete node b (Result v) -> Bool
nocondition _ = True


type OptimalPower node = Map Idx.State [(Idx.PPos node)]



optimalPower :: [(Idx.State, [(Idx.PPos node)])] -> OptimalPower node
optimalPower = Map.fromList
{-
  List.foldr f Map.empty 
  where f (s, lst) = 
          Map.insertWith' (++) s (map (uncurry (XIdxState.power s)) lst)
-}

type OptimalEtaWithEnv node f v = Map Idx.State (Map (Idx.PPos node) (Map (f v) (v, v)))

data OptimalEnvParams node f g v = OptimalEnvParams {
  etaMap :: Map String (v -> v),
  points :: Sweep.Points f g v,
  optimalPowers :: OptimalPower node,
  stateFlowGraph :: StateFlowGraph node }
