module EFA.Graph.Topology (
   Topology,
   FlowTopology,
   ClassifiedTopology,
   pathExists,
   flowFromPlain,
   plainFromFlow,
   dirEdgeFromStructureEdge,
   structureEdgeFromDirEdge,
   isStorage,
   maybeStorage,
   isActive,
   anyActive,
   StoreDir(..),
   classifyStorages,
   viewNodeDir,
   ViewNodeDir(..),
   InOut,
   ) where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph as Graph
import EFA.Graph (Graph)

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Foldable as Fold
import Control.Monad (mplus)
import Data.Maybe.HT (toMaybe)


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


isActive :: Graph.EitherEdge node -> Bool
isActive (Graph.EUnDirEdge _) = False
isActive (Graph.EDirEdge _) = True

anyActive :: Map (Graph.EitherEdge node) () -> Bool
anyActive = Fold.any isActive . Map.keysSet


{-
Should Topology have an UnDirEdge?
UnDirEdge could be read as "canonically oriented" edge.
-}
type Topology a = Graph a Graph.DirEdge (Node.Type ()) ()

type FlowTopology a = Graph a Graph.EitherEdge (Node.Type ()) ()

type
   ClassifiedTopology a =
      Graph a Graph.EitherEdge (Node.Type (Maybe StoreDir)) ()


pathExists :: (Ord a) => a -> a -> Topology a -> Bool
pathExists src dst =
   let go topo a =
          not (Graph.isEmpty topo) &&
          (a==dst ||
           (any (go (Graph.delNode a topo)) $ Graph.suc topo a))
   in  flip go src


flowFromPlain :: (Ord node) => Topology node -> FlowTopology node
flowFromPlain = Graph.mapEdgesMaybe (Just . Graph.EDirEdge)

plainFromFlow :: (Ord node) => FlowTopology node -> Topology node
plainFromFlow =
   Graph.mapEdgesMaybe
      (\e ->
         Just $
         case e of
            Graph.EDirEdge de -> de
            Graph.EUnDirEdge ue -> Graph.DirEdge (Graph.from ue) (Graph.to ue))

structureEdgeFromDirEdge :: Graph.DirEdge node -> Idx.StructureEdge node
structureEdgeFromDirEdge (Graph.DirEdge x y) = Idx.StructureEdge x y

dirEdgeFromStructureEdge :: Idx.StructureEdge node -> Graph.DirEdge node
dirEdgeFromStructureEdge (Idx.StructureEdge x y) = Graph.DirEdge x y


data StoreDir = In | Out deriving (Eq, Ord, Show)

type InOut node nodeLabel =
        (Map (Graph.EitherEdge node) (),
         nodeLabel,
         Map (Graph.EitherEdge node) ())


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
