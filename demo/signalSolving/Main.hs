module Main where

import EFA.Example.Utility ( edgeVar, makeEdges, constructSeqTopo )
import EFA.Example.Absolute ( (.=) )

import qualified EFA.Signal.Data as Data
import EFA.Signal.Data (Data(Data))

import qualified EFA.Example.Absolute as EqGen
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import Data.Monoid (mconcat)


sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0

node0, node1, node2, node3 :: Node.Int
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom $ minBound


topoDreibein :: TD.Topology Node.Int
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Sink),
              (node2, TD.Crossing),
              (node3, TD.Storage)]
        es = [(node0, node2), (node1, node2), (node2, node3)]

signal :: [a] -> Data.List a
signal = Data

scalar :: a -> Data.Scalar a
scalar = Data


given :: EqGen.EquationSystem Node.Int s (Data.Scalar Double) (Data.List Double)
given =
   mconcat $

   (Idx.DTime sec0 .= signal [1/3, 1/3, 1/3]) :
   (Idx.DTime sec1 .= signal [1]) :
   (Idx.DTime sec2 .= signal [2/3, 1/3]) :

   (Idx.Storage (Idx.SecNode sec2 node3) .= scalar 10.0) :


   (edgeVar Idx.Power sec0 node2 node3 .= signal [4.0, 3.8, 4.2]) :

   (edgeVar Idx.X sec0 node2 node3 .= signal [0.34, 0.32, 0.30]) :

   (edgeVar Idx.Power sec1 node3 node2 .= signal [5]) :
   (edgeVar Idx.Power sec2 node3 node2 .= signal [6, 6]) :

   (edgeVar Idx.Eta sec0 node3 node2 .= signal [0.25, 0.25, 0.25]) :
   (edgeVar Idx.Eta sec0 node2 node3 .= signal [0.25, 0.25, 0.25]) :
   (edgeVar Idx.Eta sec0 node2 node1 .= signal [0.50, 0.50, 0.50]) :
   (edgeVar Idx.Eta sec0 node0 node2 .= signal [0.75, 0.75, 0.75]) :
   []


main :: IO ()
main = do

  let seqTopo = constructSeqTopo topoDreibein [1, 0, 1]
      env = EqGen.solve seqTopo given

  Draw.sequFlowGraphAbsWithEnv "" seqTopo env

