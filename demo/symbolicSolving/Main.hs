module Main where

import EFA.Example.Utility (edgeVar, makeEdges, constructSeqTopo)
import EFA.Example.Absolute ((=<>))

import qualified EFA.Symbolic.SumProduct as SumProduct
import qualified EFA.Example.Absolute as EqGen

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import Data.Monoid (mempty)


sec0 :: Idx.Section
sec0 = Idx.Section 0

node0, node1, node2, node3 :: Node.Int
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom minBound


topoDreibein :: TD.Topology Node.Int
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Sink),
              (node2, TD.Crossing),
              (node3, TD.Storage)]
        es = [(node0, node2), (node1, node2), (node2, node3)]


type SignalTerm = EqGen.SignalTerm SumProduct.Term Node.Int
type ScalarTerm = EqGen.ScalarTerm SumProduct.Term Node.Int


given :: EqGen.EquationSystem Node.Int s ScalarTerm SignalTerm
given =
   Idx.DTime Idx.initSection =<>
   Idx.DTime sec0 =<>

   Idx.Storage (Idx.SecNode sec0 node3) =<>

   edgeVar Idx.Power sec0 node3 node2 =<>
   edgeVar Idx.X sec0 node2 node3 =<>

   edgeVar Idx.Eta sec0 node3 node2 =<>
   edgeVar Idx.Eta sec0 node0 node2 =<>
   edgeVar Idx.Eta sec0 node2 node1 =<>

   mempty


main :: IO ()
main = do

  let seqTopo = constructSeqTopo topoDreibein [1]
      env = EqGen.solve seqTopo given

  Draw.sequFlowGraphAbsWithEnv seqTopo env
