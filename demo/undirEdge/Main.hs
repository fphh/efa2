
module Main where

import qualified EFA.Application.Index as XIdx
import EFA.Application.Utility (makeEdges, constructSeqTopo)
import EFA.Application.Absolute ((.=))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Application.Absolute as EqGen
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import Data.Monoid (mconcat)


sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0

node0, node1, node2, node3 :: Node.Int
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom minBound


topoDreibein :: TD.Topology Node.Int
topoDreibein = Gr.fromList ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Sink),
              (node2, TD.Crossing),
              (node3, TD.storage)]
        es = [(node0, node2), (node1, node2), (node2, node3)]


given :: EqGen.EquationSystem Node.Int s Double Double
given =
   mconcat $

   (XIdx.dTime sec0 .= 1) :
   (XIdx.dTime sec1 .= 1) :

   (XIdx.storage Idx.initial node3 .= 10.0) :

   (XIdx.power sec0 node0 node2 .= 4.0) :
   (XIdx.eta sec0 node2 node1 .= 0.5) :
   (XIdx.eta sec0 node0 node2 .= 0.75) :

   (XIdx.power sec1 node3 node2 .= 4.0) :
   (XIdx.eta sec1 node2 node3 .= 0.25) :
   (XIdx.eta sec1 node0 node2 .= 0.75) :



   []

main :: IO ()
main = do

  let seqTopo = constructSeqTopo topoDreibein [2, 3]
      env = EqGen.solve seqTopo given

  Draw.xterm $ Draw.sequFlowGraphAbsWithEnv seqTopo env
