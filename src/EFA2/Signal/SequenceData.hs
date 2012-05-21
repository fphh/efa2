{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EFA2.Signal.SequenceData where

import qualified Data.Map as M

-- import EFA2.Interpreter.Arith
import EFA2.Topology.TopologyData

import EFA2.Signal.Base
import EFA2.Signal.Signal

-----------------------------------------------------------------------------------
-- | Indices for Record, Section and Power Position
newtype RecIdx = RecIdx Int deriving (Show,Eq,Ord) -- dataset Index
newtype SecIdx = SecIdx Int deriving (Show,Eq,Ord, Num, Enum)
data PPosIdx =  PPosIdx !Int !Int  deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------------
-- | Signal Record & Power Record & Flow -- Structure for Handling recorded data

type PPosData a = M.Map PPosIdx a


-- | Signal record to contain original time signals 
data Record = Record TSig SignalMap deriving (Show)
data SigId = SigId String deriving (Show, Eq, Ord)
type SignalMap = M.Map SigId (UTSig Val)

-- | Power record to contain power signals assigned to the tree
type PPosPowers = PPosData PSig
data PowerRecord = PowerRecord TSig PPosPowers deriving (Show)
type SequPwrRecord = SequData [PowerRecord]

-- | Flow record to contain flow signals assigned to the tree
type PPosFlows = PPosData FSig
data FlowRecord = FlowRecord DTSig (PPosData FSig) deriving (Show)
type SequFlowRecord = SequData [FlowRecord]

-- | Flow record to contain flow signals assigned to the tree
data FlowValRecord = FlowValRecord DTVal (PPosData FVal) deriving (Show)
type SequFlowValRecord = SequData [FlowValRecord]

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
