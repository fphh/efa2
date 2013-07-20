{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
       StateFlowGraph,
       DirStateFlowGraph,
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
       viewNodeDir,
       ViewNodeDir(..),
       ) where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph as Gr
import EFA.Graph (Graph)


import Control.Monad (mplus)
import Data.Foldable (Foldable, foldMap)
import Data.Maybe.HT (toMaybe)
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)
import Data.Monoid ((<>))

import qualified Test.QuickCheck as QC


type LNode a = Gr.LNode (Idx.AugSecNode a) (NodeType ())
type LEdge a = Gr.LEdge (FlowEdge Gr.EitherEdge) (Idx.AugSecNode a) ()
type LDirNode a = StNode (Maybe StoreDir) a
type LDirEdge a = Gr.LEdge Gr.DirEdge (Idx.AugSecNode a) ()
type StNode store a = Gr.LNode (Idx.AugSecNode a) (NodeType store)

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


isStructureEdge :: Eq node => FlowEdge structEdge (Idx.AugNode sec node) -> Bool
isStructureEdge e = case edgeType e of StructureEdge _ -> True ; _ -> False

isStorageEdge :: Eq node => FlowEdge structEdge (Idx.AugNode sec node) -> Bool
isStorageEdge e = case edgeType e of StorageEdge _ -> True ; _ -> False


data EdgeType (inPart :: (* -> *) -> * -> *) part structEdge node =
     StructureEdge (inPart structEdge node)
   | StorageEdge (Idx.ForNode (Idx.StorageEdge part) node)
   deriving (Eq, Ord, Show)

data FlowEdge structEdge augNode =
        (AugNode augNode, Part (PartOf augNode)) =>
           FlowEdge {
              edgeType ::
                 EdgeType
                    (InPartOf (PartOf augNode)) (PartOf augNode)
                    structEdge (NodeOf augNode)
           }


instance Gr.Edge structEdge => Foldable (FlowEdge structEdge) where
   foldMap f e = f (Gr.from e) <> f (Gr.to e)

instance Gr.Edge structEdge => Gr.Edge (FlowEdge structEdge) where
   from (FlowEdge e) =
      case e of
         StructureEdge sn ->
            flip switchInPart sn $ \ sec se -> secNode sec $ Gr.from se
         StorageEdge (Idx.ForNode (Idx.StorageEdge sec _) node) ->
            augNode (Idx.allowExit sec) node
   to (FlowEdge e) =
      case e of
         StructureEdge sn ->
            flip switchInPart sn $ \ sec se -> secNode sec $ Gr.to se
         StorageEdge (Idx.ForNode (Idx.StorageEdge _ sec) node) ->
            augNode (Idx.allowInit sec) node


instance
   (Eq node, Eq (structEdge node)) =>
      Eq (FlowEdge structEdge (Idx.AugSecNode node)) where
   (==) = equating edgeType

instance
   (Ord node, Ord (structEdge node)) =>
      Ord (FlowEdge structEdge (Idx.AugSecNode node)) where
   compare = comparing edgeType

instance
   (Eq node, Eq (structEdge node)) =>
      Eq (FlowEdge structEdge (Idx.AugStateNode node)) where
   (==) = equating edgeType

instance
   (Ord node, Ord (structEdge node)) =>
      Ord (FlowEdge structEdge (Idx.AugStateNode node)) where
   compare = comparing edgeType



class Part sec where
   type InPartOf sec :: (* -> *) -> * -> *
   switchInPart :: (sec -> idx node -> a) -> InPartOf sec idx node -> a

instance Part Idx.Section where
   type InPartOf Idx.Section = Idx.InSection
   switchInPart f (Idx.InSection sec idx) = f sec idx

instance Part Idx.State where
   type InPartOf Idx.State = Idx.InState
   switchInPart f (Idx.InState sec idx) = f sec idx


class AugNode augNode where
   type PartOf augNode :: *
   type NodeOf augNode :: *
   secNode :: PartOf augNode -> NodeOf augNode -> augNode
   augNode :: Idx.Augmented (PartOf augNode) -> NodeOf augNode -> augNode

instance AugNode (Idx.AugNode sec node) where
   type PartOf (Idx.AugNode sec node) = sec
   type NodeOf (Idx.AugNode sec node) = node
   secNode = Idx.TimeNode . Idx.augment
   augNode = Idx.TimeNode



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
      Graph (Idx.AugSecNode a) (FlowEdge Gr.EitherEdge) (NodeType (Maybe StoreDir)) ()

type
   DirSequFlowGraph a =
      Graph (Idx.AugSecNode a) (FlowEdge Gr.DirEdge) (NodeType (Maybe StoreDir)) ()


type
   StateFlowGraph a =
      Graph
         (Idx.AugStateNode a) (FlowEdge Gr.EitherEdge)
         (NodeType (Maybe StoreDir)) ()

type
   DirStateFlowGraph a =
      Graph
         (Idx.AugStateNode a) (FlowEdge Gr.DirEdge)
         (NodeType (Maybe StoreDir)) ()


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
   Gr.nmapWithInOut
      (\(pre, (_n, nt), suc) ->
         let maybeDir es cls =
                toMaybe (any (isActive . fst) es) cls
         in  fmap (\() -> mplus (maybeDir pre In) (maybeDir suc Out)) nt)

data ViewNodeDir node =
     ViewNodeIn  (Idx.TimeNode Idx.InitOrSection node)
   | ViewNodeOut (Idx.TimeNode Idx.SectionOrExit node)

viewNodeDir ::
   (Idx.AugSecNode node, Maybe StoreDir) ->
   Maybe (ViewNodeDir node)
viewNodeDir (node, mdir) =
   flip fmap mdir $ \dir ->
      case dir of
         In ->
            maybe (error "Exit node must be Out storage") ViewNodeIn $
            Idx.maybeExitNode node
         Out ->
            maybe (error "Init node must be In storage") ViewNodeOut $
            Idx.maybeInitNode node



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
