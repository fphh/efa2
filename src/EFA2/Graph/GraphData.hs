{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, RankNTypes, FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graph.GraphData
-- Copyright   :  (c) Dr. Heinrich Hördegen
-- 
-- Maintainer  : hoerdegen@funktional.info
--
-- Data type and class definitions needed througout the rest of the modules.
-- 
-----------------------------------------------------------------------------


module EFA2.Graph.GraphData where


import EFA2.Graph.Graph
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV

import Debug.Trace
import Data.Maybe (fromJust)

import EFA2.Signal.SignalData


-----------------------------------------------------------------------------------
-- Maps Idexes and Index Method to store topology related data
         
         -- When indexing for eta signals we don't care about xy or yx,
-- because it's the same edge in the graph.
-- The difference is only important when indexing for sample signals,
-- because there are two sample signals per edge.

-- Generic Map on all Nodes
newtype NodeData a = NodeData (M.Map NodeIndex a)  deriving (Show,Eq)
data NodeIndex = NodeIndex !Int deriving (Show, Ord, Eq)

-- Generic Map on all Edges
newtype EdgeData a = EdgeData (M.Map EdgeIndex a)  deriving (Show,Eq)
data EdgeIndex = EdgeIndex !Int !Int deriving  (Show)

-- Generic Map on all Power Positions
newtype PPosData a = PPosData (M.Map PPosIndex a)  deriving (Show,Eq)
data PPosIndex = PPosIndex !Int !Int deriving (Show, Ord, Eq)

-- Generic Map on all End-Nodes
newtype EndNodeData a = EndNodeData (M.Map StoIndex a)  deriving (Show,Eq)
data EndNodeIndex =  EndNodeIndex !Int deriving  (Show, Ord, Eq)

-- Generic Map on all Storages
newtype StoData a = StoData (M.Map StoIndex a)  deriving (Show,Eq)
data StoIndex = StoIndex !Int deriving  (Show, Ord, Eq)

-- Generic Map on all Crossing Nodes
newtype CrossNodeData a = CrossNodeData (M.Map StoIndex a)  deriving (Show,Eq)
data CrossNodeIndex =  CrossNodeIndex !Int deriving  (Show, Ord, Eq)
   
-----------------------------------------------------------------------------------
-- Classes to allow Indexing of Edges and Power Positions

instance Eq EdgeIndex where
         (EdgeIndex a b) == (EdgeIndex x y) = f a b == f x y
           where f u v = if u < v then [u, v] else [v, u]

instance Ord EdgeIndex where
         compare as@(EdgeIndex a b) bs@(EdgeIndex x y)
           | as == bs = EQ
           | otherwise = compare (a, b) (x, y)

class IndexC a where
      mkIdx :: Int -> Int -> a

instance IndexC EdgeIndex where
         mkIdx = EdgeIndex

instance IndexC PPosIndex where
         mkIdx = PPosIndex


-----------------------------------------------------------------------------------
-- Topology Dataset -- given Topology with Names & Constraints
data TopoSet = TopoSet { topo :: TopoGraph,
                         nameEnv :: NodeData String,
                         constraints  :: EdgeData  AllowedFlow} deriving (Show) -- Maybe TODO add constraints for Nodes as well as another field

data AllowedFlow = PermitNeg | PermitZero | PermitPos deriving (Show, Ord, Eq)

-----------------------------------------------------------------------------------
-- Flow State Topology Data -- Flow State with propagation Direction Analysis
data StateSet = StateSet { stateEnv :: EdgeData FlowState,
                           propEnv :: EdgeData PropDir} deriving (Show) -- TODO -- add flowchange terms here

data FlowState = Negative | Zero | Positive deriving (Show, Ord, Eq)

-- absolute PropDir
data PropDir = PropNeg  | PropZero | PropPos deriving (Show, Ord, Eq)

-----------------------------------------------------------------------------------
-- Signal Map -- assign Power signals (eventually later with calculation instructions)

data SignalMap = SignalMap   {sigMap   :: PPosData (SigIdx, Sign)}
data Sign = Leave | Flip

-----------------------------------------------------------------------------------
-- Flow State - Numeric Data flow data in flow direction of actual state -- TODO -- generate Bi-Flow Structure as well 

data UniFlowSet = UniFlowSet   { dtimeEnv :: UV.Vector DTSample,
                                 flowEnv :: PPosData (UV.Vector ESample),
                                 etaEnv  :: EdgeData  (UV.Vector EEta),
                                 stoEnv  :: EdgeData  (UV.Vector XSample)}
                                                    
-- -----------------------------------------------------------------------------------
-- -- Numeric Flow Values
-- data DeltaSet =  DeltaSet     { dflowEnv :: PPosData ESample,
--                                 detaEnv :: EtaData EEta}
--                                 --flowChange :: DFlowEnv}       -- numeric flowChange vals to add here                                              

