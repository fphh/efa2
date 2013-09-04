module EFA.Test.Tree where

import qualified EFA.TestUtility as Test

import qualified EFA.Application.Topology.TripodA as Tripod
import qualified EFA.Application.Tree as Tree
import EFA.Application.Topology.TripodA (Node, node0, node1, node2, node3)
import EFA.Application.Tree ( (<+) )

import qualified EFA.Graph.Topology as Topo


tree :: Topo.Topology Node
tree =
   Tree.toGraph $
   Tree.storage node3
      <+ (Tree.crossing node2
             <+ Tree.sink node1
             <+ Tree.source node0)


runTests :: IO ()
runTests =
   Test.single "Tree syntax" $
      Tripod.topology == tree
