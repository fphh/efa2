module EFA.Graph.Topology (
       NLabel (..), LNode,
       LEdge,
       NodeType (..),
       EdgeType (..),
       FlowDirection (..),
       EdgeLabel,
       EdgeTypeField,      getEdgeType,
       FlowDirectionField, getFlowDirection,
       Topology,
       FlowTopology,
       SecTopology,
       SequFlowGraph,
       DirSequFlowGraph,
       pathExists,
       isStorage,
       isActive,
       isInactive,
       isActiveEdge,
       isInactiveEdge,
       edgeType,
       isOriginalEdge,
       isIntersectionEdge,
       isDirEdge, isStorageNode,
       defaultNLabel,
       InOut,
       StoreDir(..),
       getActiveStores) where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph as Gr
import EFA.Graph (Graph, mkInOutGraphFormat)

import qualified Test.QuickCheck as QC
import qualified Data.Map as M
import Control.Monad (mplus)
import Data.Maybe.HT (toMaybe)
import Data.Tuple.HT (snd3)


type LNode = Gr.LNode Idx.SecNode NodeType
type LEdge = Gr.LEdge Idx.SecNode FlowDirection

data NodeType = Storage
              | Sink
              | AlwaysSink
              | Source
              | AlwaysSource
              | Crossing
              | DeadNode
              | NoRestriction deriving (Show, Ord, Eq)

isStorage :: NodeType -> Bool
isStorage nt =
   case nt of
      Storage -> True
      _ -> False


newtype NLabel = NLabel { nodeType :: NodeType } deriving (Show, Eq, Ord)

defaultNLabel :: NLabel
defaultNLabel = NLabel NoRestriction


data FlowDirection = Dir | UnDir deriving (Show, Eq, Ord)

isActive :: FlowDirection -> Bool
isActive UnDir = False
isActive _ = True

isInactive :: FlowDirection -> Bool
isInactive = not . isActive

data EdgeType = OriginalEdge
              | IntersectionEdge deriving (Eq, Ord, Show)


edgeType :: Gr.Edge Idx.SecNode -> EdgeType
edgeType e =
   if isOriginalEdge e
     then OriginalEdge
     else IntersectionEdge

isActiveEdge :: FlowDirectionField el => el -> Bool
isActiveEdge = isActive . getFlowDirection

isInactiveEdge :: FlowDirectionField el => el -> Bool
isInactiveEdge = isInactive . getFlowDirection


class EdgeTypeField el where
   getEdgeType :: el -> EdgeType

class FlowDirectionField el where
   getFlowDirection :: el -> FlowDirection

class (EdgeTypeField el, FlowDirectionField el) => EdgeLabel el where


instance EdgeTypeField EdgeType where
   getEdgeType = id

{-
instance FlowDirectionField EdgeType where
   getFlowDirection _ = Dir

instance EdgeLabel EdgeType where
-}


instance FlowDirectionField FlowDirection where
   getFlowDirection = id

instance FlowDirectionField l => FlowDirectionField (e, l) where
   getFlowDirection = getFlowDirection . snd


class MakeSecNode n where
   makeSecNode :: n -> Idx.SecNode

instance MakeSecNode Idx.SecNode where
   makeSecNode = id

class SecEdgeField e where
   getSecEdge :: e -> Gr.Edge Idx.SecNode

instance MakeSecNode n => SecEdgeField (Gr.Edge n) where
   getSecEdge = fmap makeSecNode

instance SecEdgeField e => EdgeTypeField (e, l) where
   getEdgeType (e, _l) = edgeType $ getSecEdge e


isOriginalEdge :: Gr.Edge Idx.SecNode -> Bool
isOriginalEdge (Gr.Edge (Idx.SecNode sx _) (Idx.SecNode sy _))  =  sx == sy

isIntersectionEdge :: Gr.Edge Idx.SecNode -> Bool
isIntersectionEdge (Gr.Edge (Idx.SecNode sx _) (Idx.SecNode sy _))  =  sx /= sy


isDirEdge :: FlowDirectionField label => (a, label) -> Bool
isDirEdge = dir . getFlowDirection . snd
  where dir Dir = True
        dir _ = False

type Topology = Graph Idx.Node NodeType ()

type FlowTopology = Graph Idx.Node NodeType FlowDirection

type SecTopology = Graph Idx.SecNode NodeType FlowDirection

type SequFlowGraph = Graph Idx.SecNode NodeType FlowDirection

type DirSequFlowGraph = Graph Idx.SecNode NodeType ()

pathExists :: Idx.Node -> Idx.Node -> FlowTopology -> Bool
pathExists _ _ topo | Gr.isEmpty topo = False
pathExists a b topo | a == b = True 
pathExists a b topo = or $ map f s
  where s = map fst $ filter q $ Gr.lsuc topo a
        q (_, Dir) = True
        q _ = False
        f x = pathExists x b (Gr.delNode topo a)


type InOut n el = ([Gr.LNode n el], [Gr.LNode n el])

isStorageNode :: (a, (b, NodeType), c) -> Bool
isStorageNode = isStorage . snd . snd3


-- | Active storages, grouped by storage number, sorted by section number.
getActiveStores ::
   (FlowDirectionField el) =>
   Graph Idx.SecNode NodeType el ->
   M.Map Idx.Node (M.Map Idx.Section (InOut Idx.SecNode el, StoreDir))
getActiveStores =
   M.fromListWith
      (M.unionWith (error "the same storage multiple times in a section")) .
   map
      (\(pre, (sn@(Idx.SecNode s n), _nt), suc) ->
         (n, let inout = (pre, suc)
             in  case maybeActiveSt sn inout of
                    Nothing -> M.empty
                    Just dir -> M.singleton s (inout, dir))) .
   filter isStorageNode .
   mkInOutGraphFormat

-- | Classify the storages in in and out storages,
-- looking only at edges, not at values.
-- This means that nodes with in AND out edges cannot be treated.
maybeActiveSt ::
   (FlowDirectionField el) =>
   Idx.SecNode -> InOut Idx.SecNode el -> Maybe StoreDir
maybeActiveSt n (ins, outs) =
   mplus
      (toMaybe
         (any (\(m,l) -> isActiveEdge l && isOriginalEdge (Gr.Edge m n)) ins)
         In)
      (toMaybe (any (isActiveEdge . snd) outs) Out)


data StoreDir = In | Out deriving (Eq, Show)


instance QC.Arbitrary NodeType where
   arbitrary =
      QC.oneof $ map return $
         Storage :
         Sink :
         AlwaysSink :
         Source :
         AlwaysSource :
         Crossing :
         DeadNode :
         NoRestriction :
         []

   shrink NoRestriction = []
   shrink AlwaysSink = [Sink]
   shrink AlwaysSource = [Source]
   shrink _ = [NoRestriction]
