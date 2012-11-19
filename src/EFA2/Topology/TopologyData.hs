module EFA2.Topology.TopologyData (
       NLabel (..), LNode,
       ELabel (..), LEdge,
       NodeType (..),
       EdgeType (..),
       FlowDirection (..),
       Topology,
       FlowTopology,
       SecTopology,
       SequFlowGraph,
       isStorage,
       isActive,
       isInactive,
       isActiveEdge,
       isInactiveEdge,
       isOtherSection,
       isOriginalEdge,
       isInnerStorageEdge,
       isIntersectionEdge,
       flipFlowDirection,
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


data FlowDirection = WithDir
                   | AgainstDir
                   | UnDir deriving (Show, Eq, Ord)

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
defaultELabel = ELabel OriginalEdge WithDir

isActiveEdge :: ELabel -> Bool
isActiveEdge = isActive . flowDirection

isInactiveEdge :: ELabel -> Bool
isInactiveEdge = isInactive . flowDirection


isOriginalEdge :: ELabel -> Bool
isOriginalEdge = (OriginalEdge ==) . edgeType

isInnerStorageEdge :: ELabel -> Bool
isInnerStorageEdge = (InnerStorageEdge ==) . edgeType

isIntersectionEdge :: ELabel -> Bool
isIntersectionEdge = (IntersectionEdge ==) . edgeType

flipFlowDirection :: FlowDirection -> FlowDirection
flipFlowDirection WithDir = AgainstDir
flipFlowDirection AgainstDir = WithDir
flipFlowDirection UnDir = UnDir


type Topology = EfaGraph Idx.Node NodeType ()

type FlowTopology = EfaGraph Idx.Node NodeType FlowDirection

type SecTopology = EfaGraph Idx.SecNode NodeType FlowDirection

type SequFlowGraph = EfaGraph Idx.SecNode NodeType ELabel


type InOut n el = ([Gr.LNode n el], [Gr.LNode n el])

-- | Active storages, grouped by storage number, sorted by section number.
getActiveStores ::
   SequFlowGraph ->
   M.Map Idx.Node (M.Map Idx.Section (InOut Idx.SecNode ELabel))
getActiveStores =
   M.map (M.filter isActiveSt) .
   M.fromListWith
      (M.unionWith (error "the same storage multiple times in a section")) .
   map
      (\(pre, (Idx.SecNode s n, _nt), suc) ->
         (n, M.singleton s (pre, suc))) .
   filter (isStorage . snd . snd3) .
   mkInOutGraphFormat

isActiveSt :: InOut n ELabel -> Bool
isActiveSt (ins, outs) =
   any isActiveEdge $ map snd $ ins ++ outs


data StoreDir = In | Out deriving (Eq, Show)

-- | Partition the storages in in and out storages, looking only at edges, not at values.
-- This means that nodes with in AND out edges cannot be treated.
partitionInOutStatic ::
   (Ord sec) =>
   M.Map sec (InOut n ELabel) ->
   (M.Map sec (InOut n ELabel), M.Map sec (InOut n ELabel))
partitionInOutStatic = M.partition ((In ==) . classifyInOutStatic)

classifyInOutStatic :: InOut n ELabel -> StoreDir
classifyInOutStatic (ins, outs)  =
   if any q ins /= any r outs
     then In
     else Out
   where q (_, e) =
            flowDirection e == WithDir &&
            (isOriginalEdge e || isInnerStorageEdge e)
         r (_, e) =
            flowDirection e == AgainstDir &&
            (isOriginalEdge e || isInnerStorageEdge e)
