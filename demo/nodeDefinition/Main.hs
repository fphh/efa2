-- | Demonstriert, wie man einen Knotendatentyp definiert und den Knoten spezielle Labels zuweist oder einfach den vordefinierten Knotentyp benützt.

module Main where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.Stream (Stream((:~)))
import EFA.Graph (mkGraph)
import qualified EFA.Graph.Topology.Nodes as N
import qualified EFA.Graph.Draw as Draw
import EFA.Example.Utility (makeEdges)


sec0 :: Idx.Section
sec0 :~ _ = Stream.enumFrom $ Idx.Section 0

-------------------------------------------------
-- Vordefinierte Knoten

node0, node1 :: N.Nodes
node0 :~ node1 :~ _ = Stream.enumFrom $ N.Node 0

topo1 :: TD.Topology N.Nodes
topo1 = mkGraph nodes (makeEdges edges)
  where nodes = [(node0, TD.AlwaysSink), (node1, TD.AlwaysSource)]
        edges = [(node0, node1)]

-------------------------------------------------
-- Selbstdefinierte Knoten

data Nodes = Source | Sink deriving (Eq, Ord, Show)

instance N.ShowNode Nodes where
         showNode Source = "Dies ist eine Quelle."
         showNode Sink = "Dies ist eine Senke."

topo2 :: TD.Topology Nodes
topo2 = mkGraph nodes (makeEdges edges)
  where nodes = [(Sink, TD.AlwaysSink), (Source, TD.AlwaysSource)]
        edges = [(Source, Sink)]


main :: IO ()
main =
  concurrentlyMany_ [
    Draw.topology topo1,
    Draw.topology topo2 ]