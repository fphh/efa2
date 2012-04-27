{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EFA2.Signal.SequenceData where

import qualified Data.Map as M

import EFA2.Interpreter.Arith
import EFA2.Topology.TopologyData

-- Time Signal & Samples
type Signal =  Container Val

type Power = Signal
type Time = Signal

type PSample = Val
type TSample = Val

-- Flow Signals and Samples
type FSignal = Container Val

type Flow = FSignal 
type DTime = FSignal  
  
type DTSample = Val -- Time step
type FPSample = Val -- Flow Power

type SignalIdx = Int


data Sign = PSign | ZSign | NSign deriving (Show, Eq)

-- | determine Signal Sign  
sign :: (Eq a, Ord a, Num a) => a -> Sign
sign x | x > 0 = PSign
       | x == 0 = ZSign -- TODO add intervalls later on Zero - Detection       
       | x < 0 = NSign



-----------------------------------------------------------------------------------
-- | Indices for Record, Section and Power Position
newtype RecIdx = RecIdx Int deriving (Show,Eq,Ord) -- dataset Index
newtype SecIdx = SecIdx Int deriving (Show,Eq,Ord, Num, Enum)
data PPosIdx =  PPosIdx !Int !Int  deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------------
-- | Signal Record & Power Record & Flow -- Structure for Handling recorded data

type PPosData a = M.Map PPosIdx a


-- | Signal record to contain original time signals 
data Record = Record Time SignalMap deriving (Show,Eq)
data SigId = SigId String deriving (Show, Eq, Ord)
type SignalMap = M.Map SigId Signal

-- | Power record to contain power signals assigned to the tree
type PPosPowers = PPosData Power
data PowerRecord = PowerRecord Time PPosPowers deriving (Show)
type SequPwrRecord = SequData [PowerRecord]

-- | Flow record to contain flow signals assigned to the tree
type PPosFlows = PPosData Flow
data FlowRecord = FlowRecord Time (PPosFlows) deriving (Show)
type SequFlowRecord = SequData [FlowRecord]

newtype FlowState = FlowState (PPosData Sign) deriving (Show)
type SequFlowState = SequData [FlowState]
type SequFlowTops = SequData [FlowTopology]

-----------------------------------------------------------------------------------
-- Section and Sequence -- Structures to handle Sequence Information and Data
-- | Section analysis result
type Sequ = [Sec] 
type Sec = (SignalIdx,SignalIdx)


-- | Sequence Vector to Store Section Data  
data SequData a = SequData a deriving (Show) -- deriving Show

instance Functor SequData where
         fmap f (SequData xs) = SequData (f xs)
