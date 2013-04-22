{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
module EFA.Graph.Topology (
       NLabel (..), LNode, LDirNode, StNode,
       LEdge, LDirEdge,
       NodeType (..), storage,
       EdgeType (..),
       FlowEdge (FlowEdge),
       FlowDirection (..),
       EdgeLabel,
       FlowDirectionField, getFlowDirection,
       Topology,
       FlowTopology,
       ClassifiedTopology,
       SequFlowGraph,
       DirSequFlowGraph,
       pathExists,
       dirFromSequFlowGraph,
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

import Control.Monad (mplus)
import Data.Foldable (Foldable, foldMap)
import Data.Maybe.HT (toMaybe)
import Data.Tuple.HT (snd3)
import Data.Monoid ((<>))

import qualified Test.QuickCheck as QC


type LNode a = Gr.LNode (Idx.BndNode a) (NodeType ())
type LEdge a = Gr.LEdge FlowEdge (Idx.BndNode a) FlowDirection
type LDirNode a = Gr.LNode (Idx.BndNode a) (NodeType (Maybe StoreDir))
type LDirEdge a = Gr.LEdge Gr.DirEdge (Idx.BndNode a) ()
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
     StructureEdge (Idx.InSection Idx.StructureEdge node)
   | StorageEdge (Idx.ForNode Idx.StorageEdge node)
   deriving (Eq, Ord, Show)

data FlowEdge bndNode =
        BndNode bndNode => FlowEdge {edgeType :: EdgeType (NodeOf bndNode)}


instance Foldable FlowEdge where
   foldMap f e = f (Gr.from e) <> f (Gr.to e)

instance Gr.Edge FlowEdge where
   from (FlowEdge e) =
      case e of
         StructureEdge (Idx.InSection sec (Idx.StructureEdge node _)) ->
            afterSecNode sec node
         StorageEdge (Idx.ForNode (Idx.StorageEdge bnd _) node) ->
            bndNode bnd node
   to (FlowEdge e) =
      case e of
         StructureEdge (Idx.InSection sec (Idx.StructureEdge _ node)) ->
            afterSecNode sec node
         StorageEdge (Idx.ForNode (Idx.StorageEdge _ bnd) node) ->
            bndNode bnd node

instance (EqEdge bndNode) => Eq (FlowEdge bndNode) where
   (==)  =  eqEdge

class EqEdge bndNode where
   eqEdge :: FlowEdge bndNode -> FlowEdge bndNode -> Bool

instance (Eq node) => EqEdge (Idx.BndNode node) where
   eqEdge (FlowEdge x) (FlowEdge y)  =  x==y


instance (OrdEdge bndNode) => Ord (FlowEdge bndNode) where
   compare  =  cmpEdge

class EqEdge bndNode => OrdEdge bndNode where
   cmpEdge :: FlowEdge bndNode -> FlowEdge bndNode -> Ordering

instance (Ord node) => OrdEdge (Idx.BndNode node) where
   cmpEdge (FlowEdge x) (FlowEdge y)  =  compare x y


instance (ShowEdge bndNode) => Show (FlowEdge bndNode) where
   showsPrec  =  showEdge

class ShowEdge bndNode where
   showEdge :: Int -> FlowEdge bndNode -> ShowS

instance (Show node) => ShowEdge (Idx.BndNode node) where
   showEdge p (FlowEdge e) = showsPrec p e


class BndNode bndNode where
   type NodeOf bndNode :: *
   sectionFromBndNode :: bndNode -> Idx.Boundary
   nodeFromBndNode :: bndNode -> NodeOf bndNode
   afterSecNode :: Idx.Section -> NodeOf bndNode -> bndNode
   bndNode :: Idx.Boundary -> NodeOf bndNode -> bndNode

instance BndNode (Idx.BndNode node) where
   type NodeOf (Idx.BndNode node) = node
   sectionFromBndNode (Idx.BndNode bnd _node) = bnd
   nodeFromBndNode (Idx.BndNode _bnd node) = node
   afterSecNode = Idx.afterSecNode
   bndNode = Idx.BndNode


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


isStructureEdge :: Eq node => FlowEdge (Idx.BndNode node) -> Bool
isStructureEdge e = case edgeType e of StructureEdge _ -> True ; _ -> False

isStorageEdge :: Eq node => FlowEdge (Idx.BndNode node) -> Bool
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

type Topology a = Graph a Gr.DirEdge (NodeType ()) ()

type FlowTopology a = Graph a Gr.DirEdge (NodeType ()) FlowDirection

type
   ClassifiedTopology a =
      Graph a Gr.DirEdge (NodeType (Maybe StoreDir)) FlowDirection

type
   SequFlowGraph a =
      Graph (Idx.BndNode a) FlowEdge (NodeType (Maybe StoreDir)) FlowDirection

type
   DirSequFlowGraph a =
      Graph (Idx.BndNode a) FlowEdge (NodeType (Maybe StoreDir)) ()


pathExists :: (Ord a) => a -> a -> FlowTopology a -> Bool
pathExists src dst =
   let go topo a =
          not (Gr.isEmpty topo) &&
          (a==dst ||
           (any (go (Gr.delNode a topo)) $ Gr.suc topo a))
   in  flip go src . Gr.lefilter isDirEdge

{-
In principle, we could remove "dead nodes", but
then the storage equations would not work.
Therefore we should not remove "dead nodes"
iff they are storages.
Anyway, I don't remove dead nodes,
because it will make DirSequFlowGraph more complicated
or the generation of storage equations will be more complicated.
-}
dirFromSequFlowGraph ::
   (Ord node) =>
   SequFlowGraph node -> DirSequFlowGraph node
dirFromSequFlowGraph =
   Gr.emap (const ()) . Gr.lefilter isDirEdge


type InOut e n el = ([Gr.LEdge e n el], [Gr.LEdge e n el])

isStorageNode :: (a, (b, NodeType sl), c) -> Bool
isStorageNode = isStorage . snd . snd3


classifyStorages ::
   (FlowDirectionField el, Ord node) =>
   Graph node Gr.DirEdge (NodeType ()) el ->
   Graph node Gr.DirEdge (NodeType (Maybe StoreDir)) el
classifyStorages =
   Gr.nmapWithInOut
      (\(pre, (_n, nt), suc) ->
         fmap (\() -> maybeActiveSt (pre, suc)) nt)

-- | Classify the storages in in and out storages,
-- looking only at edges, not at values.
-- This means that nodes with in AND out edges cannot be treated.
maybeActiveSt ::
   (FlowDirectionField el) =>
   InOut e n el -> Maybe StoreDir
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
