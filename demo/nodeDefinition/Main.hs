-- | Demonstriert, wie man einen Knotendatentyp definiert und den Knoten spezielle Labels zuweist oder einfach den vordefinierten Knotentyp benützt.

module Main where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Utility.Stream as Stream
import qualified EFA.Report.Format as Format
import EFA.Graph (mkGraph)
import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.Stream (Stream((:~)))
import EFA.Example.Utility (makeEdges)


sec0 :: Idx.Section
sec0 :~ _ = Stream.enumFrom $ Idx.Section 0

-------------------------------------------------
-- Vordefinierte Knoten Node

node00, node01 :: Node.Int
node00 :~ node01 :~ _ = Stream.enumFrom $ Node.Int 0

topo0 :: TD.Topology Node.Int
topo0 = mkGraph nodes (makeEdges edges)
  where nodes = [(node00, TD.AlwaysSink), (node01, TD.AlwaysSource)]
        edges = [(node00, node01)]


-------------------------------------------------
-- Vordefinierte Knoten Int

node10, node11 :: Node.Int
node10 :~ node11 :~ _ = Stream.enumFrom $ Node.Int 0

topo1 :: TD.Topology Node.Int
topo1 = mkGraph nodes (makeEdges edges)
  where nodes = [(node10, TD.AlwaysSink), (node11, TD.AlwaysSource)]
        edges = [(node10, node11)]


-------------------------------------------------
-- Vordefinierte Knoten String

node20, node21 :: Node.String
node20 = Node.String "node20"
node21 = Node.String "node21"

topo2 :: TD.Topology Node.String
topo2 = mkGraph nodes (makeEdges edges)
  where nodes = [(node20, TD.AlwaysSink), (node21, TD.AlwaysSource)]
        edges = [(node20, node21)]


-------------------------------------------------
-- Selbstdefinierte Knoten mit default show-Funktion

data NodesAB = A | B deriving (Eq, Ord, Enum, Show)

instance Node.C NodesAB where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault

topo3 :: TD.Topology NodesAB
topo3 = mkGraph nodes (makeEdges edges)
  where nodes = [(A, TD.AlwaysSink), (B, TD.AlwaysSource)]
        edges = [(A, B)]

-------------------------------------------------
-- Selbstdefinierte Knoten mit selbstdefinierter show-Funktion

data Nodes = Source | Sink deriving (Eq, Ord, Enum, Show)

instance Node.C Nodes where
   display Source = Format.literal "Dies ist eine Quelle."
   display Sink   = Format.literal "Dies ist eine Senke."

   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault


topo4 :: TD.Topology Nodes
topo4 = mkGraph nodes (makeEdges edges)
  where nodes = [(Sink, TD.AlwaysSink), (Source, TD.AlwaysSource)]
        edges = [(Source, Sink)]

-------------------------------------------------

main :: IO ()
main =
  concurrentlyMany_ [
    Draw.topology topo0,
    Draw.topology topo1,
    Draw.topology topo2,
    Draw.topology topo3,
    Draw.topology topo4 ]
