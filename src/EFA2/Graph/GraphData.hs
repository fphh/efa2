{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, RankNTypes, FlexibleContexts, FunctionalDependencies, UndecidableInstances #-}

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
import EFA2.Signal.Sequence


-----------------------------------------------------------------------------------
-- Maps Idexes and Index Method to store topology related data
         
         -- When indexing for eta signals we don't care about xy or yx,
-- because it's the same edge in the graph.
-- The difference is only important when indexing for sample signals,
-- because there are two sample signals per edge.

-- Generic Map on all Nodes
--newtype NodeEnv a = NodeEnv (M.Map NodeIndex a)  deriving (Show,Eq)

-- Generic Map on all Edges
--newtype EdgeData a = EdgeData (M.Map EdgeIndex a)  deriving (Show,Eq)

{-
-- Generic Map on all Power Positions
newtype MPointData a = MPointData (M.Map MPointIndex a)  deriving (Show,Eq)
data MPointIndex = MPointIndex !Int !Int deriving (Show, Ord, Eq)

mPointIndexFlip :: MPointIndex -> MPointIndex
mPointIndexFlip (MPointIndex idx1 idx2) = MPointIndex idx2 idx1

-}
{-
data NodeIdx = NodeIdx !Int deriving (Show, Ord, Eq)
data EdgeIdx = EdgeIdx !Int !Int deriving  (Show)

type NodeEnv a = M.Map NodeIdx a
type EdgeEnv a = M.Map EdgeIdx a

-----------------------------------------------------------------------------------
-- Classes to allow Indexing of Edges and Power Positions

instance Eq EdgeIndex where
         (EdgeIndex a b) == (EdgeIndex x y) = f a b == f x y
           where f u v = if u < v then [u, v] else [v, u]

instance Ord EdgeIndex where
         compare as@(EdgeIndex a b) bs@(EdgeIndex x y)
           | as == bs = EQ
           | otherwise = compare (a, b) (x, y)

mkIdx x y = EdgeIndex x y
-}

{-
instance IndexC MPointIndex
         where mkIdx = MPointIndex


-- type safe indexing on Graph data
class (GIndexible a b) where 
instance GIndexible (NodeData a) b
instance GIndexible (EdgeData a) b
instance GIndexible (MPointData a) b

-- Class to combine Index and Container & perform safe lookup
class GIndexPair a b c | a -> b, b-> a, a b -> c where
  (~!) :: b -> a -> c 

instance GIndexPair NodeIndex (NodeData c) c where 
  (~!) (NodeData map) idx =  maybe err id (M.lookup idx map)
    where err = error ("Error in NodeData Lookup - Item not found :" ++ show idx)

instance GIndexPair EdgeIndex (EdgeData c) c where 
  (~!) (EdgeData map) idx =  maybe err id (M.lookup idx map)
    where err = error ("Error in EdgeData Lookup - Item not found :" ++ show idx)

instance GIndexPair MPointIndex (MPointData c) c where 
  (~!) (MPointData map) idx =  maybe err id (M.lookup idx map)
    where err = error ("Error in MPointData Lookup - Item not found :" ++ show idx)

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

data Mapping = Mapping  (MPointData (SignalIdent, FlipSign))
data FlipSign = DontFlip | Flip

-----------------------------------------------------------------------------------
-- Flow State - Numeric Data flow data in flow direction of actual state -- TODO -- generate Bi-Flow Structure as well 

data SigData = SigData  {sdTime :: Time,
                         sdSigs :: MPointData (Power)}

data FlowData cont = FlowData {dtime :: cont DTSample, 
                              flow :: MPointData (cont ESample), 
                              eta  :: EdgeData (cont NSample), 
                              sto  :: NodeData (cont ESample)}
                                  

-- -----------------------------------------------------------------------------------
-- -- Numeric Delta Flow Values
-- data DeltaSet =  DeltaSet     { dflowEnv :: PPosData ESample,
--                                 detaEnv :: EtaData EEta}
--                                 --flowChange :: DFlowEnv}       -- numeric flowChange vals to add here                                              

instance Display EdgeIndex where disp (EdgeIndex idx1 idx2) = "E" ++ show idx1 ++"-" ++ show idx2
instance Display NodeIndex where disp (NodeIndex idx) = "N" ++ show idx

-}