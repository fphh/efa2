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
data PPosIdx =  PPosIdx !Int !Int deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------------
-- | Signal Record & Power Record & Flow -- Structure for Handling recorded data

-- | Signal record to contain original time signals 
data Record = Record TSig (M.Map SigId UTSigL) deriving (Show)
data SigId = SigId String deriving (Show, Eq, Ord)

-- | Power record to contain power signals assigned to the tree
data PowerRecord = PowerRecord TSigL (M.Map PPosIdx PSigL) deriving (Show)

-- | Power Record to contain Power signals after cutting
data SecPowerRecord = SecPowerRecord TSig (M.Map PPosIdx PSig) deriving (Show)


type SequPwrRecord = SequData [SecPowerRecord]

-- | Flow record to contain flow signals assigned to the tree
data FlRecord a b = FlRecord a (M.Map PPosIdx b)
type FlowRecord = FlRecord DTFSig FFSig
type FlowValRecord = FlRecord DTVal FVal


{-
-- | Flow record to contain flow signals assigned to the tree
type PPosFlows = PPosData FSig
data FlowRecord = FlowRecord DTSig (PPosData FSig) deriving (Show)

data FlowValRecord = FlowValRecord DTVal (PPosData FVal) deriving (Show)
-}

type SequFlowRecord a = SequData [a]

-- | Flow record to contain flow signals assigned to the tree
--type SequFlowValRecord = SequData [FlowValRecord]

newtype FlowState = FlowState (M.Map PPosIdx Sign) deriving (Show)
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
