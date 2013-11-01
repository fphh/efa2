{-# LANGUAGE Rank2Types #-}
module EFA.Application.OneStorage where

import qualified EFA.Application.Sweep as Sweep

import qualified EFA.Flow.State.Quantity as StateFlow

import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result)

import qualified Data.Map as Map; import Data.Map (Map)


data SocDrive a = NoDrive
                | ChargeDrive a
                | DischargeDrive a deriving (Show, Eq)


noforcing ::
  (Arith.Constant v) =>
  SocDrive v -> StateFlow.Graph node b (Result v) -> v
noforcing _ _ = Arith.zero


nocondition :: StateFlow.Graph node b (Result v) -> Bool
nocondition _ = True


type OptimalPower node = Map Idx.State [(TopoIdx.PPos node)]



optimalPower :: [(Idx.State, [(TopoIdx.PPos node)])] -> OptimalPower node
optimalPower = Map.fromList
{-
  List.foldr f Map.empty 
  where f (s, lst) = 
          Map.insertWith' (++) s (map (uncurry (XIdxState.power s)) lst)
-}

type OptimalEtaWithEnv node f v = Map Idx.State (Map (TopoIdx.PPos node) (Map (f v) (v, v)))

data OptimalEnvParams node f g v = OptimalEnvParams {
  etaMap :: Map String (v -> v),
  points :: Sweep.Points f g v,
  optimalPowers :: OptimalPower node
  }
