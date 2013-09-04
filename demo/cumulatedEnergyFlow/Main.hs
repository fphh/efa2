module Main where

import qualified EFA.Application.Topology.TripodA as Tripod
import qualified EFA.Application.Absolute as EqGen
import EFA.Application.Topology.TripodA (Node, node0, node1, node2, node3)
import EFA.Application.Utility ( constructSeqTopo )
import EFA.Application.Absolute ( (.=) )

import qualified EFA.Flow.Sequence.Index as XIdx

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Draw as Draw
import EFA.Graph.CumulatedFlow (cumulate)

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.Stream (Stream((:~)))

import Data.Monoid (Monoid, mconcat)


sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0


given :: EqGen.EquationSystem Node s Double Double
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
   (XIdx.power sec3 node3 node2 .= 7) :
   (XIdx.power sec4 node3 node2 .= 8) :

   (XIdx.eta sec0 node3 node2 .= 0.25) :
   (XIdx.eta sec0 node2 node1 .= 0.5) :
   (XIdx.eta sec0 node0 node2 .= 0.75) :

   (XIdx.eta sec1 node3 node2 .= 0.25) :
   (XIdx.eta sec1 node2 node1 .= 0.5) :
   (XIdx.eta sec1 node0 node2 .= 0.75) :
   (XIdx.power sec1 node1 node2 .= 4.0) :


   (XIdx.eta sec2 node3 node2 .= 0.75) :
   (XIdx.eta sec2 node2 node1 .= 0.5) :
   (XIdx.eta sec2 node0 node2 .= 0.75) :
   (XIdx.power sec2 node1 node2 .= 4.0) :

   (XIdx.eta sec1 node2 node3 .= 0.25) :

   []


main :: IO ()
main = do

  let seqTopo = constructSeqTopo Tripod.topology [1, 0, 1]
      env = EqGen.solve seqTopo given
      (with, against) = cumulate Tripod.topology seqTopo env

  concurrentlyMany_ $ map Draw.xterm [
    Draw.sequFlowGraphAbsWithEnv seqTopo env,
    uncurry Draw.cumulatedFlow with,
    uncurry Draw.cumulatedFlow against ]
