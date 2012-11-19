module EFA2.Topology.TopologyData (
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
       isInnerStorageEdge,
       isIntersectionEdge,
       defaultELabel,
       defaultNLabel,
       InOut,
       StoreDir(..),
       getActiveStores,
       partitionInOutStatic,
       classifyInOutStatic) where

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Topology.EfaGraph as Gr
import EFA2.Topology.EfaGraph (EfaGraph, mkInOutGraphFormat)

import qualified Data.Map as M
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
              | InnerStorageEdge
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

isInnerStorageEdge :: EdgeTypeField et => et -> Bool
isInnerStorageEdge = (InnerStorageEdge ==) . getEdgeType

isIntersectionEdge :: EdgeTypeField et => et -> Bool
isIntersectionEdge = (IntersectionEdge ==) . getEdgeType


type Topology = EfaGraph Idx.Node NodeType ()

type FlowTopology = EfaGraph Idx.Node NodeType FlowDirection

type SecTopology = EfaGraph Idx.SecNode NodeType FlowDirection

type SequFlowGraph = EfaGraph Idx.SecNode NodeType ELabel

type DirSequFlowGraph = EfaGraph Idx.SecNode NodeType EdgeType


type InOut n el = ([Gr.LNode n el], [Gr.LNode n el])

-- | Active storages, grouped by storage number, sorted by section number.
getActiveStores ::
   (FlowDirectionField el) =>
   EfaGraph Idx.SecNode NodeType el ->
   M.Map Idx.Node (M.Map Idx.Section (InOut Idx.SecNode el))
getActiveStores =
   M.map (M.filter isActiveSt) .
   M.fromListWith
      (M.unionWith (error "the same storage multiple times in a section")) .
   map
      (\(pre, (Idx.SecNode s n, _nt), suc) ->
         (n, M.singleton s (pre, suc))) .
   filter (isStorage . snd . snd3) .
   mkInOutGraphFormat

isActiveSt ::
   (FlowDirectionField el) => InOut n el -> Bool
isActiveSt (ins, outs) =
   any isActiveEdge $ map snd $ ins ++ outs


data StoreDir = In | Out deriving (Eq, Show)

-- | Partition the storages in in and out storages, looking only at edges, not at values.
-- This means that nodes with in AND out edges cannot be treated.
partitionInOutStatic ::
   (EdgeLabel el, Ord sec) =>
   M.Map sec (InOut n el) ->
   (M.Map sec (InOut n el), M.Map sec (InOut n el))
partitionInOutStatic = M.partition ((In ==) . classifyInOutStatic)

classifyInOutStatic ::
   (EdgeLabel el) => InOut n el -> StoreDir
classifyInOutStatic (ins, _outs)  =
   if any q ins then In else Out
   where q (_, e) =
            isActiveEdge e &&
            (isOriginalEdge e || isInnerStorageEdge e)
