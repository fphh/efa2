{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

module EFA.Application.Optimisation.Params where

import qualified EFA.Application.Optimisation.Balance as Balance

import qualified EFA.Application.Optimisation.Sweep as Sweep
import qualified EFA.Application.Optimisation.ReqsAndDofs as ReqsAndDofs

--import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Signal.Signal as Sig

--import qualified EFA.IO.TableParser as Table

import qualified EFA.Graph.Topology as Topology

--import qualified EFA.Equation.Arithmetic as Arith
--import EFA.Equation.Arithmetic ((~+))


--import EFA.Equation.Result (Result)
import EFA.Signal.Record(PowerRecord)

--import qualified Data.Map as Map;
import Data.Map (Map)
import Data.Vector(Vector)
--import Data.Bimap (Bimap)
--import Data.Maybe(fromMaybe)
--import Debug.Trace(trace)
import qualified EFA.IO.TableParserTypes as TPT

--import qualified EFA.Application.Optimisation.Balance as Balance

type EtaAssignMap node = Map (TopoIdx.Position node) (Name, Name)

newtype EtaFunction a b = EtaFunction {func :: (a -> b)}

instance Show (EtaFunction a b) where show _ = "EtaFunction"

newtype InitStorageState node a =
  InitStorageState { unInitStorageState :: Map node a } deriving Show

newtype InitStorageSeq node a =
  InitStorageSeq { unInitStorageSeq :: Map node a } deriving Show

newtype Name = Name { unName :: String } deriving (Eq, Ord, Show)

type OptimalEtaWithEnv node list v =
  Map Idx.State (Map (TopoIdx.Position node) (Map (list v) (v, v, v)))

data System node a = System {
  systemTopology :: Topology.Topology node,
  labeledTopology :: Topology.LabeledTopology node,
  etaAssignMap :: EtaAssignMap node,
  etaMap :: Map Name (EtaFunction a a),
  etaTable :: TPT.Map a,
  scaleTableEta :: Map Name (a, a),
  storagePositions:: [TopoIdx.Position node],
  initStorageState :: InitStorageState node a,
  initStorageSeq :: InitStorageSeq node a } deriving Show

{-
instance Ref.Data (System node a) where
  toData x = StringData "Params.System" (show x)
-}

data Optimisation node list sweep vec a = Optimisation {
--  stateFlowGraphOpt :: StateQty.Graph node (Result (sweep vec a)) (Result (sweep vec a)),
  reqsPos :: ReqsAndDofs.Reqs (TopoIdx.Position node),
  dofsPos :: ReqsAndDofs.Dofs (TopoIdx.Position node),
  points :: Map (list a)
                (ReqsAndDofs.Pair (Sweep.List sweep vec) (Sweep.List sweep vec) a),
  sweepLength :: Int,
  etaToOptimise :: Maybe (TopoIdx.Position node),
  maxEtaIterations :: MaxEtaIterations ,
  maxBalanceIterations:: MaxBalanceIterations ,
  initialBattForcing :: Balance.Forcing node a,
  initialBattForceStep :: Balance.ForcingStep node a,
  etaThreshold :: EtaThreshold a,
  balanceThreshold :: BalanceThreshold a,
  balanceForcingSeed :: Balance.SocDrive a } deriving Show

data Simulation node vec a = Simulation {
  varReqRoomPower1D :: Sig.PSignal vec a,
  varReqRoomPower2D :: Sig.PSignal2 Vector vec a,
  requirementGrid :: [Sig.PSignal vec a],
--  activeSupportPoints :: Sig.UTDistr vec ([[a]], [Sig.SignalIdx]),
  reqsRec :: PowerRecord node vec a,
  sequFilterTime :: a,
  sequFilterEnergy :: a } deriving Show


newtype MaxEtaIterations =
  MaxEtaIterations { unMaxEtaIterations :: Int } deriving Show

newtype MaxBalanceIterations =
  MaxBalanceIterations { unMaxBalanceIterations :: Int } deriving Show

newtype MaxStateIterations =
  MaxStateIterations { unMaxStateIterations :: Int } deriving Show

newtype BalanceThreshold a =
  BalanceThreshold { unBalanceThreshold :: a } deriving Show

newtype StateTimeThreshold a =
  StateTimeThreshold { unStateTimeThreshold :: a } deriving Show

newtype EtaThreshold a =
  EtaThreshold { unEtaThreshold :: a } deriving Show

newtype MaxInnerLoopIterations =
  MaxInnerLoopIterations { unMaxInnerLoopIterations :: Int } deriving Show


