-- | This example demonstrates the functionality of StateAnalysis

module Main where

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr
import EFA.Example.Utility (makeEdges)

import qualified EFA.Graph.Topology.Node as Node

data Nodes = N0 | N1 | N2 | N3 deriving (Eq, Ord, Enum, Show)

instance Node.C Nodes where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault


topoDreibein :: TD.Topology Nodes
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [(N0, TD.Source),
              (N1, TD.Sink),
              (N2, TD.Crossing),
              (N3, TD.Storage)]
        es = [(N0, N2), (N1, N2), (N2, N3)]


main :: IO ()
main = do
  let sol = StateAnalysis.advanced topoDreibein
  print (length sol)
  Draw.flowTopologies sol
