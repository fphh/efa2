module Main where

import EFA2.Example.Utility (edgeVar, makeEdges, (.=), constructSeqTopo)

import EFA2.Topology.Draw (drawTopology)

import qualified EFA2.Utils.Stream as Stream
import EFA2.Utils.Stream (Stream((:~)))

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Topology.TopologyData as TD
import qualified EFA2.Topology.EquationGenerator as EqGen
import qualified EFA2.Topology.EfaGraph as Gr

import Data.Foldable (foldMap)


sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0

node0, node1, node2, node3 :: Idx.Node
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom $ Idx.Node 0


topoDreibein :: TD.Topology
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Sink),
              (node2, TD.Crossing),
              (node3, TD.Storage)]
        es = [(node0, node2), (node1, node2), (node2, node3)]

given :: EqGen.EquationSystem s Double
given =
   foldMap (uncurry (.=)) $

   (EqGen.dtime Idx.initSection, 1) :
   (EqGen.dtime sec0, 1) :
   (EqGen.dtime sec1, 1) :
   (EqGen.dtime sec2, 1) :

   (EqGen.storage (Idx.SecNode sec2 node3), 10.0) :


   (edgeVar EqGen.power sec0 node2 node3, 4.0) :

   (edgeVar EqGen.xfactor sec0 node2 node3, 0.32) :

   (edgeVar EqGen.power sec1 node3 node2, 5) :
   (edgeVar EqGen.power sec2 node3 node2, 6) :
   (edgeVar EqGen.power sec3 node3 node2, 7) :
   (edgeVar EqGen.power sec4 node3 node2, 8) :

   (edgeVar EqGen.eta sec0 node3 node2, 0.25) :
   (edgeVar EqGen.eta sec0 node2 node3, 0.25) :
   (edgeVar EqGen.eta sec0 node2 node1, 0.5) :
   (edgeVar EqGen.eta sec0 node0 node2, 0.75) :
   []


main :: IO ()
main = do

  let seqTopo = constructSeqTopo topoDreibein [1, 0, 1] 
      env = EqGen.solveSystem given seqTopo

  drawTopology seqTopo env

