module EFA.Graph.Topology (
       NLabel (..), LNode, LDirNode, StNode,
       LEdge, LDirEdge,
       NodeType (..), storage,
       EdgeType (..),
       FlowDirection (..),
       EdgeLabel,
       FlowDirectionField, getFlowDirection,
       Topology,
       FlowTopology,
       ClassifiedTopology,
       SequFlowGraph,
       DirSequFlowGraph,
       pathExists,
       fromTopology,
       isStorage,
       maybeStorage,
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
       classifyStorages,
       ) where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph as Gr
import EFA.Graph (Graph)

import Data.Maybe.HT (toMaybe)
import Data.Tuple.HT (snd3)
import Control.Monad (mplus)

import qualified Test.QuickCheck as QC


type LNode a = Gr.LNode (Idx.BndNode a) (NodeType ())
type LEdge a = Gr.LEdge (Idx.BndNode a) FlowDirection
type LDirNode a = Gr.LNode (Idx.BndNode a) (NodeType (Maybe StoreDir))
type LDirEdge a = Gr.LEdge (Idx.BndNode a) ()
type StNode store a = Gr.LNode (Idx.BndNode a) (NodeType store)

data
   NodeType a =
        Storage a
      | Sink
      | AlwaysSink
      | Source
      | AlwaysSource
      | Crossing
      | DeadNode
      | NoRestriction
      deriving (Show, Eq, Ord)

instance Functor NodeType where
   fmap f typ =
      case typ of
         Storage a     -> Storage $ f a
         Sink          -> Sink
         AlwaysSink    -> AlwaysSink
         Source        -> Source
         AlwaysSource  -> AlwaysSource
         Crossing      -> Crossing
         DeadNode      -> DeadNode
         NoRestriction -> NoRestriction

storage :: NodeType ()
storage = Storage ()


isStorage :: NodeType sl -> Bool
isStorage nt =
   case nt of
      Storage _ -> True
      _ -> False

maybeStorage :: NodeType sl -> Maybe sl
maybeStorage nt =
   case nt of
      Storage x -> Just x
      _ -> Nothing


newtype NLabel = NLabel { nodeType :: NodeType () } deriving (Show, Eq, Ord)

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


isDirEdge :: FlowDirectionField x => x -> Bool
isDirEdge = dir . getFlowDirection
  where dir Dir = True
        dir _ = False

{-
isDirEdge :: FlowDirectionField label => (a, label) -> Bool
isDirEdge = dir . getFlowDirection . snd
  where dir Dir = True
        dir _ = False
-}

type Topology a = Graph a (NodeType ()) ()

type FlowTopology a = Graph a (NodeType ()) FlowDirection

type
   ClassifiedTopology a =
      Graph a (NodeType (Maybe StoreDir)) FlowDirection

type
   SequFlowGraph a =
      Graph (Idx.BndNode a) (NodeType (Maybe StoreDir)) FlowDirection

type
   DirSequFlowGraph a =
      Graph (Idx.BndNode a) (NodeType (Maybe StoreDir)) ()

pathExists :: (Eq a, Ord a) => a -> a -> FlowTopology a -> Bool
pathExists _ _ topo | Gr.isEmpty topo = False
pathExists a b _    | a == b = True
pathExists a b topo = any f s
  where s = map fst $ filter q $ Gr.lsuc topo a
        q (_, Dir) = True
        q _ = False
        f x = pathExists x b (Gr.delNode topo a)

{-
-- should we do it with a multiparamtypeclass?
class FromTopology t s where
      fromTopology :: t a -> s a

instance FromTopology Topology SequFlowGraph where
         fromTopology = Gr.ixmap nf . Gr.emap ef
           where ef _ = Dir
                 nf = Idx.BndNode (Idx.AfterSection (Idx.Section 0))
-}

-- name conflict with Equation.System.fromTopology?
fromTopology :: (Ord a) => ClassifiedTopology a -> SequFlowGraph a
fromTopology = Gr.ixmap nf . Gr.emap ef
  where ef = const Dir
        nf = Idx.BndNode (Idx.AfterSection (Idx.Section 0))


type InOut n el = ([Gr.LNode n el], [Gr.LNode n el])

isStorageNode :: (a, (b, NodeType sl), c) -> Bool
isStorageNode = isStorage . snd . snd3


classifyStorages ::
   (FlowDirectionField el, Ord node) =>
   Graph node (NodeType ()) el ->
   Graph node (NodeType (Maybe StoreDir)) el
classifyStorages =
   Gr.nmapWithInOut
      (\(pre, (_n, nt), suc) ->
         fmap (\() -> maybeActiveSt (pre, suc)) nt)

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


class StorageLabel a where
   arbitraryStorageLabel :: QC.Gen a

instance StorageLabel () where
   arbitraryStorageLabel = return ()


instance StorageLabel a => QC.Arbitrary (NodeType a) where
   arbitrary =
      QC.oneof $
      (fmap Storage arbitraryStorageLabel :) $
      map return $
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
