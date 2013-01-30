-- | This example demonstrates the functionality of StateAnalysis

module Main where

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr
import EFA.Example.Utility (makeEdges)

import qualified EFA.Graph.Topology.Nodes as N

data Nodes = Node0 | Node1 | Node2 | Node3 deriving (Eq, Ord, Show)

instance N.ShowNode Nodes where
         showNode = show



topoDreibein :: TD.Topology Nodes
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [(Node0, TD.Source),
              (Node1, TD.Sink),
              (Node2, TD.Crossing),
              (Node3, TD.Storage)]
        es = [(Node0, Node2), (Node1, Node2), (Node2, Node3)]


main :: IO ()
main = do
  let sol = StateAnalysis.advanced topoDreibein
  print (length sol)
  Draw.flowTopologies sol
