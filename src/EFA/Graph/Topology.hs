{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
module EFA.Graph.Topology (
       NLabel (..), LNode, LDirNode, StNode,
       LEdge, LDirEdge,
       NodeType (..), storage,
       EdgeType (..),
       FlowEdge (FlowEdge),
       Topology,
       FlowTopology,
       ClassifiedTopology,
       SequFlowGraph,
       DirSequFlowGraph,
       pathExists,
       dirFromSequFlowGraph,
       structureEdgeFromDirEdge,
       isStorage,
       maybeStorage,
       isActive,
       isInactive,
       edgeType,
       isStructureEdge,
       isStorageEdge,
       defaultNLabel,
       StoreDir(..),
       classifyStorages,
       ) where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph as Gr
import EFA.Graph (Graph)

import qualified EFA.Utility.TypeConstructor as TC

import Control.Monad (mplus)
import Data.Foldable (Foldable, foldMap)
import Data.Maybe.HT (toMaybe)
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)
import Data.Monoid ((<>))

import qualified Test.QuickCheck as QC


type LNode a = Gr.LNode (Idx.BndNode a) (NodeType ())
type LEdge a = Gr.LEdge (FlowEdge Gr.EitherEdge) (Idx.BndNode a) ()
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


isActive :: Gr.EitherEdge node -> Bool
isActive (Gr.EUnDirEdge _) = False
isActive (Gr.EDirEdge _) = True

isInactive :: Gr.EitherEdge node -> Bool
isInactive = not . isActive


isStructureEdge :: Eq node => FlowEdge structEdge (Idx.BndNode node) -> Bool
isStructureEdge e = case edgeType e of StructureEdge _ -> True ; _ -> False

isStorageEdge :: Eq node => FlowEdge structEdge (Idx.BndNode node) -> Bool
isStorageEdge e = case edgeType e of StorageEdge _ -> True ; _ -> False


data EdgeType structEdge node =
     StructureEdge (Idx.InSection structEdge node)
   | StorageEdge (Idx.ForNode Idx.StorageEdge node)
   deriving (Eq, Ord, Show)

data FlowEdge structEdge bndNode =
        BndNode bndNode =>
           FlowEdge {edgeType :: EdgeType structEdge (NodeOf bndNode)}


instance Gr.Edge structEdge => Foldable (FlowEdge structEdge) where
   foldMap f e = f (Gr.from e) <> f (Gr.to e)

instance Gr.Edge structEdge => Gr.Edge (FlowEdge structEdge) where
   from (FlowEdge e) =
      case e of
         StructureEdge (Idx.InSection sec se) ->
            afterSecNode sec $ Gr.from se
         StorageEdge (Idx.ForNode (Idx.StorageEdge bnd _) node) ->
            bndNode bnd node
   to (FlowEdge e) =
      case e of
         StructureEdge (Idx.InSection sec se) ->
            afterSecNode sec $ Gr.to se
         StorageEdge (Idx.ForNode (Idx.StorageEdge _ bnd) node) ->
            bndNode bnd node

wrapEdgeType :: EdgeType structEdge node -> EdgeType (TC.Wrap structEdge) node
wrapEdgeType et =
   case et of
      StructureEdge e -> StructureEdge $ Idx.wrapInSection e
      StorageEdge e -> StorageEdge e

instance (TC.Eq structEdge) => TC.Eq (EdgeType structEdge) where
   eq = equating wrapEdgeType

instance
   (TC.Eq structEdge, EqFlowEdge bndNode) =>
      Eq (FlowEdge structEdge bndNode) where
   (==)  =  eqFlowEdge

class EqFlowEdge bndNode where
   eqFlowEdge ::
      TC.Eq structEdge =>
      FlowEdge structEdge bndNode -> FlowEdge structEdge bndNode -> Bool

instance (Eq node) => EqFlowEdge (Idx.BndNode node) where
   eqFlowEdge (FlowEdge x) (FlowEdge y) = TC.eq x y


instance (TC.Ord structEdge) => TC.Ord (EdgeType structEdge) where
   cmp = comparing wrapEdgeType

instance
   (TC.Ord structEdge, OrdFlowEdge bndNode) =>
      Ord (FlowEdge structEdge bndNode) where
   compare  =  cmpFlowEdge

class EqFlowEdge bndNode => OrdFlowEdge bndNode where
   cmpFlowEdge ::
      TC.Ord structEdge =>
      FlowEdge structEdge bndNode -> FlowEdge structEdge bndNode -> Ordering

instance (Ord node) => OrdFlowEdge (Idx.BndNode node) where
   cmpFlowEdge (FlowEdge x) (FlowEdge y)  =  TC.cmp x y


instance (TC.Show structEdge) => TC.Show (EdgeType structEdge) where
   showsPrec p = showsPrec p . wrapEdgeType

instance
   (TC.Show structEdge, ShowFlowEdge bndNode) =>
      Show (FlowEdge structEdge bndNode) where
   showsPrec  =  showFlowEdge

class ShowFlowEdge bndNode where
   showFlowEdge ::
      TC.Show structEdge =>
      Int -> FlowEdge structEdge bndNode -> ShowS

instance (Show node) => ShowFlowEdge (Idx.BndNode node) where
   showFlowEdge p (FlowEdge e) = TC.showsPrec p e


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



{-
Should Topology have an UnDirEdge?
UnDirEdge could be read as "canonically oriented" edge.
-}
type Topology a = Graph a Gr.DirEdge (NodeType ()) ()

type FlowTopology a = Graph a Gr.EitherEdge (NodeType ()) ()

type
   ClassifiedTopology a =
      Graph a Gr.EitherEdge (NodeType (Maybe StoreDir)) ()

type
   SequFlowGraph a =
      Graph (Idx.BndNode a) (FlowEdge Gr.EitherEdge) (NodeType (Maybe StoreDir)) ()

type
   DirSequFlowGraph a =
      Graph (Idx.BndNode a) (FlowEdge Gr.DirEdge) (NodeType (Maybe StoreDir)) ()


pathExists :: (Ord a) => a -> a -> Topology a -> Bool
pathExists src dst =
   let go topo a =
          not (Gr.isEmpty topo) &&
          (a==dst ||
           (any (go (Gr.delNode a topo)) $ Gr.suc topo a))
   in  flip go src

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
   Gr.mapEdgesMaybe $ \fe ->
      case edgeType fe of
         StorageEdge se -> Just $ FlowEdge $ StorageEdge se
         StructureEdge (Idx.InSection sec ee) ->
            case ee of
               Gr.EDirEdge de ->
                  Just $ FlowEdge $ StructureEdge $ Idx.InSection sec de
               Gr.EUnDirEdge _ -> Nothing

structureEdgeFromDirEdge ::
   Idx.InSection Gr.DirEdge node -> Idx.InSection Idx.StructureEdge node
structureEdgeFromDirEdge (Idx.InSection s (Gr.DirEdge x y)) =
   Idx.InSection s (Idx.StructureEdge x y)


data StoreDir = In | Out deriving (Eq, Show)

{- |
Classify the storages in in and out storages,
looking only at edges, not at values.
This means that nodes with in AND out edges cannot be treated.
-}
classifyStorages ::
   (Ord node) =>
   FlowTopology node -> ClassifiedTopology node
classifyStorages =
   Gr.nmapWithInOut
      (\(pre, (_n, nt), suc) ->
         let maybeDir es cls =
                toMaybe (any (isActive . fst) es) cls
         in  fmap (\() -> mplus (maybeDir pre In) (maybeDir suc Out)) nt)


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
