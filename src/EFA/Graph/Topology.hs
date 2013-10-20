module EFA.Graph.Topology (
   Topology,
   FlowTopology,
   flowFromPlain,
   plainFromFlow,
   dirEdgeFromStructureEdge,
   structureEdgeFromDirEdge,
   isStorage,
   isActive,
   anyActive,
   StoreDir(..),
   InOut,
   ) where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph as Graph
import EFA.Graph (Graph)

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Foldable as Fold


isStorage :: Node.Type -> Bool
isStorage nt =
   case nt of
      Node.Storage -> True
      _ -> False


isActive :: Graph.EitherEdge node -> Bool
isActive (Graph.EUnDirEdge _) = False
isActive (Graph.EDirEdge _) = True

anyActive :: Map (Graph.EitherEdge node) () -> Bool
anyActive = Fold.any isActive . Map.keysSet


{-
Should Topology have an UnDirEdge?
UnDirEdge could be read as "canonically oriented" edge.
-}
type Topology a = Graph a Graph.DirEdge () ()

type FlowTopology a = Graph a Graph.EitherEdge () ()


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
