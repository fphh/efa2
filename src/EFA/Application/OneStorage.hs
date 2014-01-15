{-# LANGUAGE Rank2Types #-}
module EFA.Application.OneStorage where

import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.ReqsAndDofs as ReqsAndDofs

import qualified EFA.Flow.State.Quantity as StateFlow
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Graph.Topology as Topology

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result)

import qualified Data.Map as Map; import Data.Map (Map)

-- | The 'SocDrive' data type should always contain positive values.
-- Use 'getSocDrive' to get the drive with signs corrected.
data SocDrive a =
  NoDrive               -- ^ No drive
  | ChargeDrive a       -- ^ Charging states should be prefered
  | DischargeDrive a    -- ^ Discharging states should be prefered
  deriving (Show, Eq)


getSocDrive ::
  (Arith.Sum a, Arith.Constant a) => SocDrive a -> a
getSocDrive soc =
  case soc of
       DischargeDrive x -> x
       ChargeDrive x -> Arith.negate x
       NoDrive -> Arith.zero

noforcing ::
  (Arith.Constant v) =>
  SocDrive v -> StateFlow.Graph node b (Result v) -> v
noforcing _ _ = Arith.zero


nocondition :: StateFlow.Graph node b (Result v) -> Bool
nocondition _ = True


type OptimalPower node = Map Idx.State [(TopoIdx.Position node)]



optimalPower :: [(Idx.State, [(TopoIdx.Position node)])] -> OptimalPower node
optimalPower = Map.fromList



type EtaAssignMap node = Map (TopoIdx.Position node) (Name, Name)

newtype InitStorageState node a = InitStorageState { unInitStorageState :: Map node a }
newtype InitStorageSeq node a = InitStorageSeq { unInitStorageSeq :: Map node a }


newtype Name = Name String deriving (Eq, Ord, Show)

type OptimalEtaWithEnv node f v =
  Map Idx.State (Map (TopoIdx.Position node) (Map (f v) (v, v, v)))

data OptimalEnvParams node f sweep vec a = OptimalEnvParams {
  systemTopology :: Topology.Topology node,
  initStorageState :: InitStorageState node a,
  initStorageSeq :: InitStorageSeq node a,
  etaMap :: Map Name (a -> a),
  etaAssignMap :: EtaAssignMap node,
  points :: Map (f a) (ReqsAndDofs.Pair (Sweep.List sweep vec) (Sweep.List sweep vec) a),
  forcingPerNode :: Map node (SocDrive a),
  reqsPos :: ReqsAndDofs.Reqs [TopoIdx.Position node],
  dofsPos :: ReqsAndDofs.Dofs [TopoIdx.Position node],
  sweepLength :: Int
  }
