module EFA2.Graph.Topology (
       NLabel (..), LNode,
       ELabel (..), LEdge,
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
       isStorage,
       isActive,
       isInactive,
       isActiveEdge,
       isInactiveEdge,
       isOtherSection,
       isOriginalEdge,
       isIntersectionEdge,
       isDirEdge, isStorageNode,
       defaultELabel,
       defaultNLabel,
       InOut,
       StoreDir(..),
       getActiveStores) where

import qualified EFA2.Graph.Topology.Index as Idx
import qualified EFA2.Graph as Gr
import EFA2.Graph (Graph, mkInOutGraphFormat)

import qualified Test.QuickCheck as QC
import qualified Data.Map as M
import Control.Monad (mplus)
import Data.Maybe.HT (toMaybe)
import Data.Tuple.HT (snd3)


type LNode = Gr.LNode Idx.SecNode NodeType
type LEdge = Gr.LEdge Idx.SecNode ELabel

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

isOtherSection :: LNode -> LNode -> Bool
isOtherSection (Idx.SecNode s1 _, _) (Idx.SecNode s2 _, _)  =  s1 /= s2

isActive :: FlowDirection -> Bool
isActive UnDir = False
isActive _ = True

isInactive :: FlowDirection -> Bool
isInactive = not . isActive

data EdgeType = OriginalEdge
              | IntersectionEdge deriving (Eq, Ord, Show)



data ELabel = ELabel { edgeType :: EdgeType,
                       flowDirection :: FlowDirection } deriving (Eq, Ord, Show)

defaultELabel :: ELabel
defaultELabel = ELabel OriginalEdge Dir

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

instance FlowDirectionField EdgeType where
   getFlowDirection _ = Dir

instance EdgeLabel EdgeType where


instance FlowDirectionField FlowDirection where
   getFlowDirection = id


instance EdgeTypeField ELabel where
   getEdgeType = edgeType

instance FlowDirectionField ELabel where
   getFlowDirection = flowDirection

instance EdgeLabel ELabel where


isOriginalEdge :: EdgeTypeField et => et -> Bool
isOriginalEdge = (OriginalEdge ==) . getEdgeType

isIntersectionEdge :: EdgeTypeField et => et -> Bool
isIntersectionEdge = (IntersectionEdge ==) . getEdgeType


isDirEdge :: (a, ELabel) -> Bool
isDirEdge = dir . getFlowDirection . snd
  where dir Dir = True
        dir _ = False

type Topology = Graph Idx.Node NodeType ()

type FlowTopology = Graph Idx.Node NodeType FlowDirection

type SecTopology = Graph Idx.SecNode NodeType FlowDirection

type SequFlowGraph = Graph Idx.SecNode NodeType ELabel

type DirSequFlowGraph = Graph Idx.SecNode NodeType EdgeType


type InOut n el = ([Gr.LNode n el], [Gr.LNode n el])

isStorageNode :: (a, (b, NodeType), c) -> Bool
isStorageNode = isStorage . snd . snd3


-- | Active storages, grouped by storage number, sorted by section number.
getActiveStores ::
   (EdgeLabel el) =>
   Graph Idx.SecNode NodeType el ->
   M.Map Idx.Node (M.Map Idx.Section (InOut Idx.SecNode el, StoreDir))
getActiveStores =
   M.fromListWith
      (M.unionWith (error "the same storage multiple times in a section")) .
   map
      (\(pre, (Idx.SecNode s n, _nt), suc) ->
         (n, let inout = (pre, suc)
             in  case maybeActiveSt inout of
                    Nothing -> M.empty
                    Just dir -> M.singleton s (inout, dir))) .
   filter isStorageNode .
   mkInOutGraphFormat

-- | Classify the storages in in and out storages,
-- looking only at edges, not at values.
-- This means that nodes with in AND out edges cannot be treated.
maybeActiveSt ::
   (EdgeLabel el) => InOut n el -> Maybe StoreDir
maybeActiveSt (ins, outs) =
   mplus
      (toMaybe
         (any (\e -> isActiveEdge e && isOriginalEdge e) $
          map snd ins)
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
