module Main where

import qualified EFA.Example.Topology.Tripod.Given as TripodGiven
import EFA.Example.Topology.Tripod (Node, node0, node1, node2, node3)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Sequence.Absolute ((.=))

import qualified EFA.Signal.Data as Data
import EFA.Signal.Data (Data(Data))

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import Data.Monoid (mconcat)


sec0, sec1, sec2 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ _ = Stream.enumFrom $ Idx.section0


signal :: [a] -> Data.List a
signal = Data

scalar :: a -> Data.Scalar a
scalar = Data


given ::
   EqSys.EquationSystemIgnore Node s
      (Data.Scalar Double) (Data.List Double)
given =
   mconcat $

   (XIdx.dTime sec0 .= signal [1/3, 1/3, 1/3]) :
   (XIdx.dTime sec1 .= signal [1]) :
   (XIdx.dTime sec2 .= signal [2/3, 1/3]) :

   (XIdx.storage (Idx.afterSection sec2) node3 .= scalar 10.0) :


   (XIdx.power sec0 node2 node3 .= signal [4.0, 3.8, 4.2]) :

   (XIdx.x sec0 node2 node3 .= signal [0.34, 0.32, 0.30]) :

   (XIdx.power sec1 node3 node2 .= signal [5]) :
   (XIdx.power sec2 node3 node2 .= signal [6, 6]) :

   (XIdx.eta sec0 node3 node2 .= signal [0.25, 0.25, 0.25]) :
   (XIdx.eta sec0 node2 node1 .= signal [0.50, 0.50, 0.50]) :
   (XIdx.eta sec0 node0 node2 .= signal [0.75, 0.75, 0.75]) :
   []


main :: IO ()
main =
   Draw.xterm $ Draw.seqFlowGraph Draw.optionsDefault $
      EqSys.solve TripodGiven.seqFlowGraph given
