module Main where

import qualified EFA.Example.Index as XIdx
import qualified EFA.Example.Absolute as EqGen
import EFA.Example.Utility ( makeEdges, constructSeqTopo )
import EFA.Example.Absolute ( (.=) )

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import qualified EFA.Report.Format as Format

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import Data.Monoid (mconcat)


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
topoDreibein = Gr.fromList ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Sink),
              (node2, TD.Crossing),
              (node3, TD.storage)]
        es = [(node0, node2), (node1, node2), (node2, node3)]

given :: EqGen.EquationSystem Node s Double Double
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
   (XIdx.power sec3 node3 node2 .= 7) :
   (XIdx.power sec4 node3 node2 .= 8) :

   (XIdx.eta sec0 node3 node2 .= 0.25) :
   (XIdx.eta sec0 node2 node3 .= 0.25) :
   (XIdx.eta sec0 node2 node1 .= 0.5) :
   (XIdx.eta sec0 node0 node2 .= 0.75) :
   []


main :: IO ()
main = do

  let seqTopo = constructSeqTopo topoDreibein [1, 0, 1]
      env = EqGen.solve seqTopo given

  Draw.sequFlowGraphAbsWithEnv "" seqTopo env

