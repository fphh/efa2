module EFA.Test.Tree where

import qualified EFA.TestUtility as Test

import qualified EFA.Application.Tree as Tree
import EFA.Application.Utility ( makeEdges )
import EFA.Application.Tree ( (<+) )

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))


node0, node1, node2, node3 :: Node.Int
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom minBound


topoDreibein :: TD.Topology Node.Int
topoDreibein = Gr.fromList ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Sink),
              (node2, TD.Crossing),
              (node3, TD.storage)]
        es = [(node0, node2), (node1, node2), (node2, node3)]

treeDreibein :: TD.Topology Node.Int
treeDreibein =
   Tree.toGraph $
   Tree.storage node3
      <+ (Tree.crossing node2
             <+ Tree.sink node1
             <+ Tree.source node0)


runTests :: IO ()
runTests =
   Test.single "Tree syntax" $
      topoDreibein == treeDreibein
