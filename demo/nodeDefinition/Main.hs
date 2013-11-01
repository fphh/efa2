{- |
Demonstriert, wie man einen Knotendatentyp definiert
und den Knoten Namen zuweist
oder einfach vordefinierte Knotentypen benützt.
-}

module Main where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Flow.Draw as Draw
import qualified EFA.Report.Format as Format
import qualified EFA.Utility.Stream as Stream
import EFA.Application.Utility (topologyFromEdges)
import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.Stream (Stream((:~)))


sec0 :: Idx.Section
sec0 :~ _ = Stream.enumFrom $ Idx.section0

-------------------------------------------------
-- Vordefinierte Knoten Int

node00, node01 :: Node.Int
node00 = Node.intAlwaysSink 0
node01 = Node.intAlwaysSource 0

topo0 :: Topo.Topology Node.Int
topo0 = topologyFromEdges [(node00, node01)]


-------------------------------------------------
-- Vordefinierte Knoten String

node10, node11 :: Node.String
node10 = Node.String Node.AlwaysSink "node10"
node11 = Node.String Node.AlwaysSource "node11"

topo1 :: Topo.Topology Node.String
topo1 = topologyFromEdges [(node10, node11)]


-------------------------------------------------
-- Selbstdefinierte Knoten mit Standard-Anzeigefunktion

data NodeAB = A | B deriving (Eq, Ord, Enum, Show)

instance Node.C NodeAB where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault
   typ A = Node.AlwaysSink
   typ B = Node.AlwaysSource

topo2 :: Topo.Topology NodeAB
topo2 = topologyFromEdges [(A, B)]

-------------------------------------------------
-- Selbstdefinierte Knoten mit selbstdefinierter Anzeigefunktion

data Node = Source | Sink deriving (Eq, Ord, Enum, Show)

instance Node.C Node where
   display Source = Format.literal "Dies ist immer\n eine Quelle."
   display Sink   = Format.literal "Dies ist immer\n eine Senke."

   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault

   typ Sink   = Node.AlwaysSink
   typ Source = Node.AlwaysSource

topo3 :: Topo.Topology Node
topo3 = topologyFromEdges [(Source, Sink)]

-------------------------------------------------

main :: IO ()
main =
  concurrentlyMany_ $ map Draw.xterm $
    Draw.topology topo0 :
    Draw.topology topo1 :
    Draw.topology topo2 :
    Draw.topology topo3 :
    []
