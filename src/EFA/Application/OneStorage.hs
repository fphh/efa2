{-# LANGUAGE Rank2Types #-}
module EFA.Application.OneStorage where

import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.ReqsAndDofs as ReqsAndDofs

import qualified EFA.Flow.State.Quantity as StateQty
import EFA.Flow.State(states)
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Signal.Signal as Sig



import qualified EFA.Graph.Topology as Topology

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result)
import EFA.Signal.Record(PowerRecord)

import qualified Data.Map as Map; import Data.Map (Map)
import Data.Vector(Vector)
import qualified Data.Vector.Unboxed as UV


-- | The 'SocDrive' data type should always contain positive values.
-- Use 'getSocDrive' to get the drive with signs corrected.
data SocDrive a =
  NoDrive               -- ^ No drive
  | ChargeDrive a       -- ^ Charging states should be prefered
  | DischargeDrive a    -- ^ Discharging states should be prefered
  deriving (Show, Eq)


--type ForcingPerNode node a = Map node (SocDrive a)

getSocDrive ::
  (Arith.Sum a, Arith.Constant a) => SocDrive a -> a
getSocDrive soc =
  case soc of
       DischargeDrive x -> Arith.negate x
       ChargeDrive x -> x
       NoDrive -> Arith.zero
       
setSocDrive ::
  (Arith.Sum a, Arith.Constant a,Ord a) => a -> SocDrive a
setSocDrive x =
  case Arith.sign x of
       Arith.Positive -> ChargeDrive x
       Arith.Zero -> NoDrive
       Arith.Negative -> DischargeDrive $ Arith.negate x
       

noforcing ::
  (Arith.Constant v) =>
  SocDrive v -> StateQty.Graph node b (Result v) -> v
noforcing _ _ = Arith.zero


data StateForcing a = StateForcing a deriving Show

instance (Arith.Sum a) => Arith.Sum (StateForcing a) where
  (~+) (StateForcing x) (StateForcing y) = StateForcing $ x Arith.~+ y
  (~-) (StateForcing x) (StateForcing y) = StateForcing $ x Arith.~- y

-- data StateForcings a = Map Idx.State (StateForcing a) deriving Show

unpackStateForcing :: StateForcing a -> a 
unpackStateForcing (StateForcing x) = x 

zeroStateForcing :: Arith.Constant a => StateQty.Graph node b (Result v) -> Map Idx.State (StateForcing a)
zeroStateForcing sg = Map.map (\_ -> StateForcing Arith.zero) $ states sg


nocondition :: StateQty.Graph node b (Result v) -> Bool
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

data OptimalEnvParams node f sweep sweepVec sigVec a = OptimalEnvParams {
  systemTopology :: Topology.Topology node,
  stateFlowGraphOpt :: StateQty.Graph node (Result (sweep sweepVec a) ) (Result (sweep sweepVec a)),
  reqsRec :: PowerRecord node sigVec a,
  initStorageState :: InitStorageState node a,
  initStorageSeq :: InitStorageSeq node a,
  etaMap :: Map Name (a -> a),
  etaAssignMap :: EtaAssignMap node,
  points :: Map (f a) (ReqsAndDofs.Pair (Sweep.List sweep sweepVec) (Sweep.List sweep sweepVec) a),
  reqsPos :: ReqsAndDofs.Reqs (TopoIdx.Position node),
  dofsPos :: ReqsAndDofs.Dofs (TopoIdx.Position node),
  etaToOptimise :: Maybe (TopoIdx.Position node),
  sweepLength :: Int,
  maxInnerLoopIterations:: MaxInnerLoopIterations ,
  initialBattForcing :: Map node (SocDrive a),
  initialBattForceStep :: Map node (SocDrive a),
  storagePositions:: [TopoIdx.Position node],
  maxEtaIterations :: MaxEtaIterations ,
  maxBalanceIterations:: MaxBalanceIterations ,
  maxStateIterations:: MaxStateIterations ,
  balanceThreshold :: BalanceThreshold a,
  stateTimeThreshold :: StateTimeThreshold a,
  etaThreshold :: EtaThreshold a,  
  stateForcingSeed :: StateForcing a,   
  balanceForcingSeed :: SocDrive a,   
  varReqRoomPower1D :: Sig.PSignal UV.Vector a,
  varReqRoomPower2D :: Sig.PSignal2 Vector UV.Vector a
  }

newtype MaxEtaIterations  =  MaxEtaIterations Int
newtype MaxBalanceIterations  = MaxBalanceIterations Int
newtype MaxStateIterations  = MaxStateIterations Int
newtype BalanceThreshold  a = BalanceThreshold a
newtype StateTimeThreshold  a = StateTimeThreshold a
newtype EtaThreshold  a = EtaThreshold a  
newtype MaxInnerLoopIterations  =  MaxInnerLoopIterations Int


type Balance node a = Map node a  
  
type StateDurations a = Map Idx.State a  
  