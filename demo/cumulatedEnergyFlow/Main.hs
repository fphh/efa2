module Main where

import EFA.Example.Utility ( edgeVar, makeEdges, constructSeqTopo )
import EFA.Example.Absolute ( (.=) )

import qualified EFA.Example.Absolute as EqGen
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import qualified EFA.Report.Format as Format

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.Stream (Stream((:~)))
import EFA.Graph.CumulatedFlow (cumulate)

import Data.Monoid (Monoid, mconcat)


sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0

node0, node1, node2, node3 :: Node
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom $ Node 0

newtype Node = Node Int deriving (Show, Eq, Ord)

instance Enum Node where
         toEnum = Node
         fromEnum (Node n) = n

instance Node.C Node where
   display (Node 0) = Format.literal "null"
   display (Node 1) = Format.literal "eins"
   display (Node 2) = Format.literal "zwei"
   display (Node 3) = Format.literal "drei"
   display n = Format.literal $ show n

   subscript (Node n) = Format.literal $ show n
   dotId = Node.dotIdDefault


topoDreibein :: TD.Topology Node
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Sink),
              (node2, TD.Crossing),
              (node3, TD.storage)]
        es = [(node0, node2), (node1, node2), (node2, node3)]

given :: EqGen.EquationSystem Node s Double Double
given =
   mconcat $

   (Idx.DTime sec0 .= 0.5) :
   (Idx.DTime sec1 .= 2) :
   (Idx.DTime sec2 .= 1) :

   (Idx.Storage (Idx.afterSecNode sec2 node3) .= 10.0) :


   (edgeVar Idx.Power sec0 node2 node3 .= 4.0) :

   (edgeVar Idx.X sec0 node2 node3 .= 0.32) :

   (edgeVar Idx.Power sec1 node3 node2 .= 5) :
   (edgeVar Idx.Power sec2 node3 node2 .= 6) :
   (edgeVar Idx.Power sec3 node3 node2 .= 7) :
   (edgeVar Idx.Power sec4 node3 node2 .= 8) :

   (edgeVar Idx.Eta sec0 node3 node2 .= 0.25) :
   (edgeVar Idx.Eta sec0 node2 node1 .= 0.5) :
   (edgeVar Idx.Eta sec0 node0 node2 .= 0.75) :

   (edgeVar Idx.Eta sec1 node3 node2 .= 0.25) :
   (edgeVar Idx.Eta sec1 node2 node1 .= 0.5) :
   (edgeVar Idx.Eta sec1 node0 node2 .= 0.75) :
   (edgeVar Idx.Power sec1 node1 node2 .= 4.0) :


   (edgeVar Idx.Eta sec2 node3 node2 .= 0.75) :
   (edgeVar Idx.Eta sec2 node2 node1 .= 0.5) :
   (edgeVar Idx.Eta sec2 node0 node2 .= 0.75) :
   (edgeVar Idx.Power sec2 node1 node2 .= 4.0) :

   (edgeVar Idx.Eta sec1 node2 node3 .= 0.25) :

   []


main :: IO ()
main = do

  let seqTopo = constructSeqTopo topoDreibein [1, 0, 1]
      env = EqGen.solve seqTopo given
      ((withTopo, withEnv), (againstTopo, againstEnv)) =
        cumulate topoDreibein seqTopo env

  concurrentlyMany_ [
    Draw.sequFlowGraphAbsWithEnv (Draw.xterm "" seqTopo) env,
    Draw.sequFlowGraphCumulated (Draw.xterm "" withTopo) withEnv,
    Draw.sequFlowGraphCumulated (Draw.xterm "" againstTopo) againstEnv ]
