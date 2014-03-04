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
import Data.Bimap (Bimap)
import Data.Maybe(fromMaybe)


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


newtype StateForcing a = StateForcing a deriving Show
data StateForcingStep a = StateForcingStep a | DontForceState deriving (Show,Eq)

instance Functor StateForcingStep where
  fmap f (StateForcingStep x) = StateForcingStep $ f x
  fmap _ (DontForceState ) = DontForceState

{-
instance (Arith.Sum a) => Arith.Sum (StateForcing a) where
  (~+) (StateForcing x) (StateForcing y) = StateForcing $ x Arith.~+ y
  (~-) (StateForcing x) (StateForcing y) = StateForcing $ x Arith.~- y
  negate (StateForcing x) = StateForcing $ Arith.negate x

instance (Arith.Product a) => Arith.Product (StateForcing a) where
  (~*) (StateForcing x) (StateForcing y) = StateForcing $ x Arith.~* y
  (~/) (StateForcing x) (StateForcing y) = StateForcing $ x Arith.~/ y
  recip (StateForcing x) = StateForcing $ Arith.recip x
  constOne (StateForcing x) = StateForcing $ Arith.constOne x
-}
type IndexConversionMap =
  Bimap Idx.State Idx.AbsoluteState


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

type OptimalEtaWithEnv node list v =
  Map Idx.State (Map (TopoIdx.Position node) (Map (list v) (v, v, v)))

data SystemParams node a = SystemParams {
  systemTopology :: Topology.Topology node,
  etaAssignMap :: EtaAssignMap node,
  etaMap :: Map Name (a -> a),
  storagePositions:: [TopoIdx.Position node],
  initStorageState :: InitStorageState node a,
  initStorageSeq :: InitStorageSeq node a}


data OptimisationParams node list sweep vec a = OptimisationParams {
  stateFlowGraphOpt :: StateQty.Graph node (Result (sweep vec a)) (Result (sweep vec a)),
  reqsPos :: ReqsAndDofs.Reqs (TopoIdx.Position node),
  dofsPos :: ReqsAndDofs.Dofs (TopoIdx.Position node),
  points :: Map (list a) (ReqsAndDofs.Pair (Sweep.List sweep vec) (Sweep.List sweep vec) a),
  sweepLength :: Int,
  etaToOptimise :: Maybe (TopoIdx.Position node),
  maxEtaIterations :: MaxEtaIterations ,
  maxInnerLoopIterations:: MaxInnerLoopIterations ,
  maxBalanceIterations:: MaxBalanceIterations ,
  maxStateIterations:: MaxStateIterations ,
  initialBattForcing :: Map node (SocDrive a),
  initialBattForceStep :: Map node (SocDrive a),
  etaThreshold :: EtaThreshold a,
  balanceThreshold :: BalanceThreshold a,
  stateTimeUpperThreshold :: StateTimeThreshold a,
  stateTimeLowerThreshold :: StateTimeThreshold a,
  stateForcingSeed :: StateForcingStep a,
  balanceForcingSeed :: SocDrive a
  }

data SimulationParams node vec a = SimulationParams {
  varReqRoomPower1D :: Sig.PSignal vec a,
  varReqRoomPower2D :: Sig.PSignal2 Vector vec a,
  requirementGrid :: [Sig.PSignal vec a],
  activeSupportPoints :: Sig.UTDistr vec ([[a]], [Sig.SignalIdx]),
  reqsRec :: PowerRecord node vec a,
  sequFilterTime :: a,
  sequFilterEnergy :: a}


newtype MaxEtaIterations  =  MaxEtaIterations Int
newtype MaxBalanceIterations  = MaxBalanceIterations Int
newtype MaxStateIterations  = MaxStateIterations Int
newtype BalanceThreshold  a = BalanceThreshold a
newtype StateTimeThreshold  a = StateTimeThreshold a
newtype EtaThreshold  a = EtaThreshold a
newtype MaxInnerLoopIterations  =  MaxInnerLoopIterations Int


type Balance node a = Map node a
type BalanceForcing node a = Map node (SocDrive a)
type BalanceForcingStep node a = Map node (SocDrive a)


type StateDurations a = Map Idx.AbsoluteState a
  
type BestBalance node a = Map node (Maybe (SocDrive a,a), Maybe (SocDrive a,a))


                        
rememberBestBalanceForcing :: 
  (Arith.Constant a, Ord a) =>
   (Maybe (SocDrive a,a), Maybe (SocDrive a,a)) -> 
  (SocDrive a,a) -> 
  (Maybe (SocDrive a,a), Maybe (SocDrive a,a))
rememberBestBalanceForcing (neg,pos) (force,bal) = 
  if bal >= Arith.zero then (neg, g pos) else (g neg, pos) 
  where
   g(Just (f,b)) = if (Arith.abs bal) >= (Arith.abs b) then Just (f,b) else Just (force,bal)
   g Nothing  = Just (force,bal)

checkCrossingEverOccured :: 
  (Maybe (SocDrive a,a), Maybe (SocDrive a,a)) -> Bool
checkCrossingEverOccured (Just _, Just _) = True
checkCrossingEverOccured (_, _) = False

getForcingIntervall ::  (Ord a, Arith.Constant a) =>
  (Maybe (SocDrive a,a), Maybe (SocDrive a,a)) 
  -> Maybe (SocDrive a)
getForcingIntervall (Just (n,_), Just (p,_)) = 
  Just $ setSocDrive ((Arith.abs $ getSocDrive n) Arith.~+ (getSocDrive p))
getForcingIntervall (_,_) = Nothing


addForcingStep :: 
  (Ord node, Ord a, Arith.Constant a) =>
  BalanceForcing node a -> 
  node -> 
  (SocDrive a) -> 
  BalanceForcing node a 
addForcingStep forcing storage step = Map.adjust f storage forcing  
  where f force = setSocDrive $ (getSocDrive force) Arith.~+ (getSocDrive step)

updateForcingStep ::
  (Ord node, Ord a, Arith.Constant a) =>
  BalanceForcingStep node a ->
  node -> 
  (SocDrive a) -> 
  BalanceForcingStep node a
updateForcingStep forcing storage step = Map.adjust f storage forcing
  where f _ = step

getStorageForcing :: 
  (Ord node, Show node) =>
  String ->  
  BalanceForcing node a ->
  node ->  SocDrive a
getStorageForcing caller balance sto = fromMaybe (error m) $ Map.lookup sto balance 
  where m = "Error in getStorageForcing called by " ++ caller 
            ++ "- gStorage not in Map : " ++ show sto
            
getStorageBalance :: 
  (Ord node, Show node) =>
  String ->  Balance node a ->
  node ->  a
getStorageBalance caller balance sto = fromMaybe (error m) $ Map.lookup sto balance 
  where m = "Error in getStorageBalance called by " ++ caller 
            ++ "- gStorage not in Map : " ++ show sto  

data StatForcing = StateForcingOn | StateForcingOff