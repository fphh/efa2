module Main where

import qualified EFA.Application.Topology.TripodA as Tripod
import EFA.Application.Topology.TripodA (Node, node0, node1, node2, node3)
import EFA.Application.Utility (seqFlowGraphFromStates)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Sequence.Absolute ( (.=) )

import qualified Data.GraphViz.Attributes.Colors.X11 as Colors

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import Data.Monoid (mconcat)


sec0, sec1, sec2 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ _ = Stream.enumFrom $ Idx.Section 0


given :: EqSys.EquationSystemIgnore Node s Double Double
given =
   mconcat $

   (XIdx.dTime sec0 .= 1) :
   (XIdx.dTime sec1 .= 1) :
   (XIdx.dTime sec2 .= 1) :

   (XIdx.storage (Idx.afterSection sec2) node3 .= 10.0) :


   (XIdx.power sec0 node2 node3 .= 4.0) :

   (XIdx.x sec0 node2 node3 .= 0.32) :

   (XIdx.power sec1 node3 node2 .= 5) :
   (XIdx.power sec2 node3 node2 .= 6) :

   (XIdx.eta sec0 node3 node2 .= 0.25) :
   (XIdx.eta sec0 node2 node1 .= 0.5) :
   (XIdx.eta sec0 node0 node2 .= 0.75) :
   []


main :: IO ()
main =
  Draw.xterm $
    Draw.bgcolour Colors.Burlywood1 $
    Draw.title "Dies ist der Titel!" $
    Draw.sequFlowGraph
       (Draw.hideEtaNode $ Draw.showStorage $ Draw.showStorageEdge $
        Draw.hideVariableIndex $ Draw.optionsDefault) $
    EqSys.solve
       (seqFlowGraphFromStates Tripod.topology [1, 0, 1])
       given
