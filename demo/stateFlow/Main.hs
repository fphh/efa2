module Main where

import qualified EFA.Application.Topology.TripodA as Tripod
import EFA.Application.Topology.TripodA (Node, node0, node1, node2, node3)
import EFA.Application.Utility (seqFlowGraphFromStates)

import qualified EFA.Flow.State.Quantity as StateFlow
import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Sequence.Absolute ( (.=) )

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.Stream (Stream((:~)))

import Data.Monoid (Monoid, mconcat)


sec0, sec1, sec2 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ _ = Stream.enumFrom $ Idx.Section 0


given :: EqSys.EquationSystemIgnore Node s Double Double
given =
   mconcat $

   (XIdx.dTime sec0 .= 0.5) :
   (XIdx.dTime sec1 .= 2) :
   (XIdx.dTime sec2 .= 1) :

   (XIdx.storage (Idx.afterSection sec2) node3 .= 10.0) :


   (XIdx.power sec0 node2 node3 .= 4.0) :

   (XIdx.x sec0 node2 node3 .= 0.32) :

   (XIdx.power sec1 node3 node2 .= 5) :
   (XIdx.power sec2 node3 node2 .= 6) :

   (XIdx.eta sec0 node3 node2 .= 0.25) :
   (XIdx.eta sec0 node2 node1 .= 0.5) :
   (XIdx.eta sec0 node0 node2 .= 0.75) :

   (XIdx.eta sec1 node2 node1 .= 0.5) :
   (XIdx.eta sec1 node0 node2 .= 0.75) :
   (XIdx.power sec1 node1 node2 .= 4.0) :


   (XIdx.eta sec2 node3 node2 .= 0.75) :
   (XIdx.eta sec2 node2 node1 .= 0.5) :
   (XIdx.eta sec2 node0 node2 .= 0.75) :
   (XIdx.power sec2 node1 node2 .= 4.0) :

   (XIdx.eta sec1 node2 node3 .= 0.25) :

   []

{-
stateEnv ::
  (Ord node) => Topo.StateFlowGraph node -> StateEnv.Complete node a v
-}


main :: IO ()
main = do

   let sequFlowGraph =
          EqSys.solve
             (seqFlowGraphFromStates Tripod.topology [1, 0, 1])
             given

       stateFlowGraph =
          StateFlow.flowGraphFromCumResult $
          StateFlow.fromSequenceFlowResult False sequFlowGraph

   concurrentlyMany_ $ map Draw.xterm $
      Draw.sequFlowGraph Draw.optionsDefault sequFlowGraph :
      Draw.stateFlowGraph Draw.optionsDefault stateFlowGraph :
      []
