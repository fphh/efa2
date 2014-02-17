


module EFA.Utility.Show where

import qualified EFA.Report.Format as Format

import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Graph.Topology.Node as Node

showNode :: (Node.C node) => node -> String
showNode node = Format.showRaw (Node.display node :: Format.ASCII)

showEdge :: (Node.C node) => TopoIdx.Position node -> String
showEdge (TopoIdx.Position a b) = showNode a ++ arrow ++ showNode b
  where arrow = " -> "
