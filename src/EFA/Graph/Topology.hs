{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
module EFA.Graph.Topology (
       NLabel (..), LNode, LDirNode, StNode,
       LEdge, LDirEdge,
       EdgeType (..),
       FlowEdge (FlowEdge),
       Topology,
       FlowTopology,
       ClassifiedTopology,
       FlowGraph,
       DirFlowGraph,
       SequFlowGraph,
       DirSequFlowGraph,
       StateFlowGraph,
       DirStateFlowGraph,
       pathExists,
       dirFromFlowGraph,
       structureEdgeFromDirEdge,
       dirEdgeFromStructureEdge,
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
       viewNodeDir,
       ViewNodeDir(..),
       ) where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph as Graph
import EFA.Graph (Graph)


import Control.Monad (mplus)
import Data.Foldable (Foldable, foldMap)
import Data.Maybe.HT (toMaybe)
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)
import Data.Monoid ((<>))


type LNode a = Graph.LNode (Idx.AugSecNode a) (Node.Type ())
type LEdge a = Graph.LEdge (FlowEdge Graph.EitherEdge) (Idx.AugSecNode a) ()
type LDirNode part a = StNode part (Maybe StoreDir) a
type LDirEdge a = Graph.LEdge Graph.DirEdge (Idx.AugSecNode a) ()
type StNode part store a = Graph.LNode (Idx.AugNode part a) (Node.Type store)


isStorage :: Node.Type sl -> Bool
isStorage nt =
   case nt of
      Node.Storage _ -> True
      _ -> False

maybeStorage :: Node.Type sl -> Maybe sl
maybeStorage nt =
   case nt of
      Node.Storage x -> Just x
      _ -> Nothing


newtype NLabel = NLabel { nodeType :: Node.Type () } deriving (Show, Eq, Ord)

defaultNLabel :: NLabel
defaultNLabel = NLabel Node.NoRestriction


isActive :: Graph.EitherEdge node -> Bool
isActive (Graph.EUnDirEdge _) = False
isActive (Graph.EDirEdge _) = True

isInactive :: Graph.EitherEdge node -> Bool
isInactive = not . isActive


isStructureEdge :: Eq node => FlowEdge structEdge (Idx.AugNode sec node) -> Bool
isStructureEdge e = case edgeType e of StructureEdge _ -> True ; _ -> False

isStorageEdge :: Eq node => FlowEdge structEdge (Idx.AugNode sec node) -> Bool
isStorageEdge e = case edgeType e of StorageEdge _ -> True ; _ -> False


data EdgeType part structEdge node =
     StructureEdge (Idx.InPart part structEdge node)
   | StorageEdge (Idx.ForNode (Idx.StorageEdge part) node)
   deriving (Eq, Ord, Show)

data FlowEdge structEdge augNode =
        (AugNode augNode) =>
           FlowEdge {
              edgeType ::
                 EdgeType (CorePartOf augNode) structEdge (NodeOf augNode)
           }


instance Graph.Edge structEdge => Foldable (FlowEdge structEdge) where
   foldMap f e = f (Graph.from e) <> f (Graph.to e)

instance Graph.Edge structEdge => Graph.Edge (FlowEdge structEdge) where
   from (FlowEdge e) =
      case e of
         StructureEdge (Idx.InPart sec se) ->
            augNode (Idx.augment sec) $ Graph.from se
         StorageEdge (Idx.ForNode (Idx.StorageEdge sec _) node) ->
            augNode (Idx.allowExit sec) node
   to (FlowEdge e) =
      case e of
         StructureEdge (Idx.InPart sec se) ->
            augNode (Idx.augment sec) $ Graph.to se
         StorageEdge (Idx.ForNode (Idx.StorageEdge _ sec) node) ->
            augNode (Idx.allowInit sec) node


instance
   (part ~ CorePartOf augNode, node ~ NodeOf augNode,
    Eq part, Eq node, Eq (structEdge node)) =>
      Eq (FlowEdge structEdge augNode) where
   (==) = equating edgeType

instance
   (part ~ CorePartOf augNode, node ~ NodeOf augNode,
    Ord part, Ord node, Ord (structEdge node)) =>
      Ord (FlowEdge structEdge augNode) where
   compare = comparing edgeType


type CorePartOf part = NoInit (NoExit (PartOf part))


class AugNode augNode where
   type PartOf augNode :: *
   type NodeOf augNode :: *
   augNode ::
      Idx.Augmented (CorePartOf augNode) -> NodeOf augNode -> augNode

instance
   (Exit part, Init (NoExit part)) =>
      AugNode (Idx.PartNode part node) where
   type PartOf (Idx.PartNode part node) = part
   type NodeOf (Idx.PartNode part node) = node
   augNode = Idx.PartNode . absorbExit . fmap absorbInit


class Exit part where
   type NoExit part :: *
   absorbExit :: Idx.Exit (NoExit part) -> part

instance Exit (Idx.Exit part) where
   type NoExit (Idx.Exit part) = part
   absorbExit = id


class Init part where
   type NoInit part :: *
   absorbInit :: Idx.Init (NoInit part) -> part

instance Init (Idx.Init part) where
   type NoInit (Idx.Init part) = part
   absorbInit = id



{-
Should Topology have an UnDirEdge?
UnDirEdge could be read as "canonically oriented" edge.
-}
type Topology a = Graph a Graph.DirEdge (Node.Type ()) ()

type FlowTopology a = Graph a Graph.EitherEdge (Node.Type ()) ()

type
   ClassifiedTopology a =
      Graph a Graph.EitherEdge (Node.Type (Maybe StoreDir)) ()


type
   FlowGraph sec node =
      Graph
         (Idx.AugNode sec node) (FlowEdge Graph.EitherEdge)
         (Node.Type (Maybe StoreDir)) ()

type
   DirFlowGraph sec node =
      Graph
         (Idx.AugNode sec node) (FlowEdge Graph.DirEdge)
         (Node.Type (Maybe StoreDir)) ()


type SequFlowGraph node = FlowGraph Idx.Section node
type DirSequFlowGraph node = DirFlowGraph Idx.Section node

type StateFlowGraph node = FlowGraph Idx.State node
type DirStateFlowGraph node = DirFlowGraph Idx.State node


pathExists :: (Ord a) => a -> a -> Topology a -> Bool
pathExists src dst =
   let go topo a =
          not (Graph.isEmpty topo) &&
          (a==dst ||
           (any (go (Graph.delNode a topo)) $ Graph.suc topo a))
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
dirFromFlowGraph ::
   (Ord part, Ord node) =>
   FlowGraph part node -> DirFlowGraph part node
dirFromFlowGraph =
   Graph.mapEdgesMaybe $ \fe ->
      case edgeType fe of
         StorageEdge se -> Just $ FlowEdge $ StorageEdge se
         StructureEdge (Idx.InPart sec ee) ->
            case ee of
               Graph.EDirEdge de ->
                  Just $ FlowEdge $ StructureEdge $ Idx.InPart sec de
               Graph.EUnDirEdge _ -> Nothing

structureEdgeFromDirEdge :: Graph.DirEdge node -> Idx.StructureEdge node
structureEdgeFromDirEdge (Graph.DirEdge x y) = Idx.StructureEdge x y

dirEdgeFromStructureEdge :: Idx.StructureEdge node -> Graph.DirEdge node
dirEdgeFromStructureEdge (Idx.StructureEdge x y) = Graph.DirEdge x y


data StoreDir = In | Out deriving (Eq, Ord, Show)

{- |
Classify the storages in in and out storages,
looking only at edges, not at values.
This means that nodes with in AND out edges cannot be treated.
-}
classifyStorages ::
   (Ord node) =>
   FlowTopology node -> ClassifiedTopology node
classifyStorages =
   Graph.mapNodeWithInOut
      (\(pre, (_n, nt), suc) ->
         let maybeDir es cls =
                toMaybe (any (isActive . fst) es) cls
         in  fmap (\() -> mplus (maybeDir pre In) (maybeDir suc Out)) nt)

data ViewNodeDir part node =
     ViewNodeIn  (Idx.PartNode (Idx.Init part) node)
   | ViewNodeOut (Idx.PartNode (Idx.Exit part) node)

viewNodeDir ::
   (Idx.AugNode part node, Maybe StoreDir) ->
   Maybe (ViewNodeDir part node)
viewNodeDir (node, mdir) =
   flip fmap mdir $ \dir ->
      case dir of
         In ->
            maybe (error "Exit node must be Out storage") ViewNodeIn $
            Idx.maybeExitNode node
         Out ->
            maybe (error "Init node must be In storage") ViewNodeOut $
            Idx.maybeInitNode node
