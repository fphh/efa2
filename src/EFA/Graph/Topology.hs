module EFA.Graph.Topology (
   Topology,
   FlowTopology,
   LabeledTopology,
   flowFromPlain,
   plainFromFlow,
   plainFromLabeled,
   dirEdgeFromTopologyEdge,
   topologyEdgeFromDirEdge,
   isActive,
   anyActive,
   StoreDir(..),
   InOut,
   ) where

import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Graph as Graph
import EFA.Graph (Graph)

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Foldable as Fold


isActive :: Graph.EitherEdge node -> Bool
isActive (Graph.EUnDirEdge _) = False
isActive (Graph.EDirEdge _) = True

anyActive :: Map (Graph.EitherEdge node) () -> Bool
anyActive = Fold.any isActive . Map.keysSet


{-
Should Topology have an UnDirEdge?
UnDirEdge could be read as "canonically oriented" edge.
-}
type Topology node = Graph node Graph.DirEdge () ()

type FlowTopology node = Graph node Graph.EitherEdge () ()

type LabeledTopology node = Graph node Graph.DirEdge String String


flowFromPlain :: (Ord node) => Topology node -> FlowTopology node
flowFromPlain = Graph.mapEdgeKeys Graph.EDirEdge

plainFromFlow :: (Ord node) => FlowTopology node -> Topology node
plainFromFlow =
   Graph.mapEdgeKeys
      (\e ->
         case e of
            Graph.EDirEdge de -> de
            Graph.EUnDirEdge ue -> Graph.DirEdge (Graph.from ue) (Graph.to ue))

plainFromLabeled :: Graph node edge nl el -> Graph node edge () ()
plainFromLabeled = Graph.mapNode (const ()) . Graph.mapEdge (const ())


topologyEdgeFromDirEdge :: Graph.DirEdge node -> TopoIdx.Edge node
topologyEdgeFromDirEdge (Graph.DirEdge x y) = TopoIdx.Edge x y

dirEdgeFromTopologyEdge :: TopoIdx.Edge node -> Graph.DirEdge node
dirEdgeFromTopologyEdge (TopoIdx.Edge x y) = Graph.DirEdge x y


data StoreDir = In | Out deriving (Eq, Ord, Show)

type InOut node nodeLabel =
        (Map (Graph.EitherEdge node) (),
         nodeLabel,
         Map (Graph.EitherEdge node) ())
