module EFA.Graph.Topology (
       NLabel (..), LNode,
       LEdge, LDirEdge,
       NodeType (..),
       EdgeType (..),
       FlowDirection (..),
       EdgeLabel,
       FlowDirectionField, getFlowDirection,
       Topology,
       FlowTopology,
       SequFlowGraph,
       DirSequFlowGraph,
       pathExists,
       isStorage,
       isActive,
       isInactive,
       isActiveEdge,
       isInactiveEdge,
       edgeType,
       isStructureEdge,
       isStorageEdge,
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


type LNode a = Gr.LNode (Idx.BndNode a) NodeType
type LEdge a = Gr.LEdge (Idx.BndNode a) FlowDirection
type LDirEdge a = Gr.LEdge (Idx.BndNode a) ()

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

data EdgeType node =
     StructureEdge (Idx.StructureEdge node)
   | StorageEdge (Idx.StorageEdge node)
   deriving (Eq, Ord, Show)


edgeType :: Eq node => Gr.Edge (Idx.BndNode node) -> EdgeType node
edgeType (Gr.Edge (Idx.BndNode sx nx) (Idx.BndNode sy ny)) =
   if sx == sy
     then
        case sx of
           Idx.AfterSection s ->
              StructureEdge $ Idx.StructureEdge s nx ny
           _ -> error "structure edges must be in a section"
     else
        if nx == ny
          then StorageEdge $ Idx.StorageEdge sx sy nx
          else error "forbidden edge type"


isActiveEdge :: FlowDirectionField el => el -> Bool
isActiveEdge = isActive . getFlowDirection

isInactiveEdge :: FlowDirectionField el => el -> Bool
isInactiveEdge = isInactive . getFlowDirection


{-
class EdgeTypeField el where
   getEdgeType :: el -> EdgeType node
-}

class FlowDirectionField el where
   getFlowDirection :: el -> FlowDirection

class ({- EdgeTypeField el, -} FlowDirectionField el) => EdgeLabel el where

{-
instance EdgeTypeField EdgeType where
   getEdgeType = id
-}

instance FlowDirectionField FlowDirection where
   getFlowDirection = id

instance FlowDirectionField l => FlowDirectionField (e, l) where
   getFlowDirection = getFlowDirection . snd


{-
class NodeEdgeType n where
   getNodeEdgeType :: Gr.Edge n -> EdgeType

instance NodeEdgeType (Idx.SecNode n) where
   getNodeEdgeType = edgeType

instance NodeEdgeType n => EdgeTypeField (Gr.Edge n) where
   getEdgeType e = getNodeEdgeType e

instance EdgeTypeField e => EdgeTypeField (e, l) where
   getEdgeType (e, _l) = getEdgeType e
-}


isStructureEdge :: Eq node => Gr.Edge (Idx.BndNode node) -> Bool
isStructureEdge e = case edgeType e of StructureEdge _ -> True ; _ -> False

isStorageEdge :: Eq node => Gr.Edge (Idx.BndNode node) -> Bool
isStorageEdge e = case edgeType e of StorageEdge _ -> True ; _ -> False


isDirEdge :: FlowDirectionField label => (a, label) -> Bool
isDirEdge = dir . getFlowDirection . snd
  where dir Dir = True
        dir _ = False

type Topology a = Graph a NodeType ()

type FlowTopology a = Graph a NodeType FlowDirection

type SequFlowGraph a = Graph (Idx.BndNode a) NodeType FlowDirection

type DirSequFlowGraph a = Graph (Idx.BndNode a) NodeType ()

pathExists :: (Eq a, Ord a) => a -> a -> FlowTopology a -> Bool
pathExists _ _ topo | Gr.isEmpty topo = False
pathExists a b _    | a == b = True
pathExists a b topo = any f s
  where s = map fst $ filter q $ Gr.lsuc topo a
        q (_, Dir) = True
        q _ = False
        f x = pathExists x b (Gr.delNode topo a)


type InOut n el = ([Gr.LNode n el], [Gr.LNode n el])

isStorageNode :: (a, (b, NodeType), c) -> Bool
isStorageNode = isStorage . snd . snd3


-- | Active storages, grouped by storage number, sorted by section number.
getActiveStores ::
   (FlowDirectionField el, Ord node) =>
   Graph node NodeType el ->
   M.Map node (InOut node el, Maybe StoreDir)
getActiveStores =
   M.fromListWith
      (error "the same storage multiple times in a section") .
   map
      (\(pre, (n, _nt), suc) ->
         (n, let inout = (pre, suc)
             in  (inout, maybeActiveSt inout))) .
   filter isStorageNode .
   mkInOutGraphFormat

-- | Classify the storages in in and out storages,
-- looking only at edges, not at values.
-- This means that nodes with in AND out edges cannot be treated.
maybeActiveSt ::
   (Eq node, FlowDirectionField el) =>
   InOut node el -> Maybe StoreDir
maybeActiveSt (ins, outs) =
   mplus
      (toMaybe (any (isActiveEdge . snd) ins)  In)
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
