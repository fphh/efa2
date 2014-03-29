{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

module EFA.Application.Optimisation.Balance where

--import qualified EFA.Application.Optimisation.Sweep as Sweep
--import qualified EFA.Application.Optimisation.ReqsAndDofs as ReqsAndDofs

import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Topology.Index as TopoIdx

--import qualified EFA.Signal.Signal as Sig



--import qualified EFA.Graph.Topology as Topology

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+))


import EFA.Equation.Result (Result)
--import EFA.Signal.Record(PowerRecord)

import qualified Data.Map as Map; import Data.Map (Map)
--import Data.Vector(Vector)
import Data.Bimap (Bimap)
import Data.Maybe(fromMaybe)
--import Debug.Trace(trace)


-- | The 'SocDrive' data type should always contain positive values.
-- Use 'getSocDrive' to get the drive with signs corrected.
data SocDrive a =
  NoDrive               -- ^ No drive
  | ChargeDrive a       -- ^ Charging states should be prefered
  | DischargeDrive a    -- ^ Discharging states should be prefered
  deriving (Show)


instance Functor SocDrive where
  fmap f (ChargeDrive x) = ChargeDrive (f x)
  fmap f (DischargeDrive x) = DischargeDrive (f x)
  fmap _ NoDrive = NoDrive

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



type IndexConversionMap =
  Bimap Idx.State Idx.AbsoluteState



nocondition :: StateQty.Graph node b (Result v) -> Bool
nocondition _ = True


type OptimalPower node = Map Idx.State [(TopoIdx.Position node)]



optimalPower :: [(Idx.State, [(TopoIdx.Position node)])] -> OptimalPower node
optimalPower = Map.fromList


type Balance node a = Map node a


data Absolute
data Step

newtype ForcingMap x a =
  ForcingMap { unForcingMap :: a } deriving (Show)

instance Functor (ForcingMap x) where
  fmap f (ForcingMap m) = ForcingMap (f m)

type Forcing node a =
  ForcingMap Absolute (Map node (SocDrive a))

type ForcingStep node a =
  ForcingMap Step (Map node (SocDrive a))


type BestBalance node a = Map node (Maybe (SocDrive a, a), Maybe (SocDrive a, a))

type StateDurations a = Map Idx.AbsoluteState a


rememberBestBalanceForcing ::
  (Arith.Constant a, Ord a, Ord node, Show node, Show a) =>
   (Maybe (SocDrive a,a), Maybe (SocDrive a,a)) ->
  (Forcing node a, Balance node a) ->
  node ->
  (Maybe (SocDrive a, a), Maybe (SocDrive a, a))
rememberBestBalanceForcing (neg, pos) (forceMap, balMap) sto =
  if bal >= Arith.zero then (neg, g pos) else (h neg, pos)
  where
    bal = getStorageBalance "rememberBestBalanceForcing" balMap sto
    force = getStorageForcing "rememberBestBalanceForcing" forceMap sto

    -- TODO :: Fix is compensating for non-Monotonic behaviour of Forcing -> Balance
    -- Correct solution would be to work with an Balance Intervall
    -- Which is calculated by maximum duration charging states to max. duration discharging states


    g (Just (f, b)) =
      if bal <= b || (getSocDrive force < getSocDrive force && bal > b)
         then Just (force, bal)
         else Just (f, b)
    g Nothing = Just (force, bal)

    h (Just (f, b)) =
      if (Arith.abs bal <= Arith.abs b) || (getSocDrive force > getSocDrive f && bal < b)
         then Just (force, bal)
         else Just (f, b)
    h Nothing = Just (force, bal)


checkCrossingEverOccured ::
  (Maybe (SocDrive a,a), Maybe (SocDrive a, a)) -> Bool
checkCrossingEverOccured (Just _, Just _) = True
checkCrossingEverOccured _ = False

getForcingIntervall ::  (Ord a, Arith.Constant a) =>
  (Maybe (SocDrive a,a), Maybe (SocDrive a,a)) ->
  Maybe (SocDrive a)
getForcingIntervall (Just (n, _), Just (p, _)) =
  Just $ setSocDrive $ (Arith.abs $ getSocDrive n) ~+ getSocDrive p
getForcingIntervall _ = Nothing


addForcingStep ::
  (Ord node, Ord a, Arith.Constant a, Show node) =>
  Forcing node a ->
  ForcingStep node a ->
  node ->
  Forcing node a
addForcingStep forcing stepMap sto =
  fmap (Map.adjust f sto) forcing
  where f force = setSocDrive $ getSocDrive force ~+ getSocDrive step
        step = getStorageForcingStep "addForcingStep" stepMap sto

updateForcingStep ::
  (Ord node, Ord a, Arith.Constant a) =>
  ForcingStep node a ->
  node ->
  SocDrive a ->
  ForcingStep node a
updateForcingStep forcing storage step =
  fmap (Map.adjust (const step) storage) forcing

getStorageForcing ::
  (Ord node, Show node) =>
  String ->
  Forcing node a ->
  node ->
  SocDrive a
getStorageForcing caller forcing sto =
  fromMaybe (error m) $ Map.lookup sto $ unForcingMap forcing
  where m = "Error in getStorageForcing called by " ++ caller
            ++ " - gStorage not in Map: " ++ show sto


getStorageForcingStep ::
  (Ord node, Show node) =>
  String ->
  ForcingStep node a ->
  node ->
  SocDrive a
getStorageForcingStep caller forcing sto =
  fromMaybe (error m) $ Map.lookup sto $ unForcingMap forcing
  where m = "Error in getStorageForcing called by " ++ caller
            ++ " - gStorage not in Map: " ++ show sto


getStorageBalance ::
  (Ord node, Show node) =>
  String ->  Balance node a ->
  node -> a
getStorageBalance caller balance sto =
  fromMaybe (error m) $ Map.lookup sto balance
  where m = "Error in getStorageBalance called by " ++ caller
            ++ " - gStorage not in Map: " ++ show sto

data StateForcing = StateForcingOn | StateForcingOff deriving (Show)



