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
   Tree.cons node3
      <+ (Tree.cons node2
             <+ Tree.cons node1
             <+ Tree.cons node0)


runTests :: IO ()
runTests =
   Test.single "Tree syntax" $
      Tripod.topology == tree
