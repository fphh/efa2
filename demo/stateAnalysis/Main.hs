-- | This example demonstrates the functionality of StateAnalysis

module Main where

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr
import EFA.Application.Utility (makeEdges)

import qualified EFA.Graph.Topology.Node as Node

data Node = N0 | N1 | N2 | N3 deriving (Eq, Ord, Enum, Show)

instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault


topoDreibein :: TD.Topology Node
topoDreibein = Gr.fromList ns (makeEdges es)
  where ns = [(N0, Node.Source),
              (N1, Node.Sink),
              (N2, Node.Crossing),
              (N3, Node.storage)]
        es = [(N0, N2), (N1, N2), (N2, N3)]


main :: IO ()
main = do
  let sol = StateAnalysis.advanced topoDreibein
  print (length sol)
  Draw.xterm $ Draw.flowTopologies sol
