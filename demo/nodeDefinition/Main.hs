{- |
Demonstriert, wie man einen Knotendatentyp definiert
und den Knoten Namen zuweist
oder einfach vordefinierte Knotentypen benützt.
-}

module Main where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Utility.Stream as Stream
import qualified EFA.Report.Format as Format
import qualified EFA.Graph as Gr
import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.Stream (Stream((:~)))
import EFA.Example.Utility (makeEdges)


sec0 :: Idx.Section
sec0 :~ _ = Stream.enumFrom $ Idx.Section 0

-------------------------------------------------
-- Vordefinierte Knoten Int

node00, node01 :: Node.Int
node00 :~ node01 :~ _ = Stream.enumFrom minBound

topo0 :: TD.Topology Node.Int
topo0 = Gr.fromList nodes (makeEdges edges)
  where nodes = [(node00, TD.AlwaysSink), (node01, TD.AlwaysSource)]
        edges = [(node00, node01)]


-------------------------------------------------
-- Vordefinierte Knoten String

node10, node11 :: Node.String
node10 = Node.String "node10"
node11 = Node.String "node11"

topo1 :: TD.Topology Node.String
topo1 = Gr.fromList nodes (makeEdges edges)
  where nodes = [(node10, TD.AlwaysSink), (node11, TD.AlwaysSource)]
        edges = [(node10, node11)]


-------------------------------------------------
-- Selbstdefinierte Knoten mit Standard-Anzeigefunktion

data NodeAB = A | B deriving (Eq, Ord, Enum, Show)

instance Node.C NodeAB where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault

topo2 :: TD.Topology NodeAB
topo2 = Gr.fromList nodes (makeEdges edges)
  where nodes = [(A, TD.AlwaysSink), (B, TD.AlwaysSource)]
        edges = [(A, B)]

-------------------------------------------------
-- Selbstdefinierte Knoten mit selbstdefinierter Anzeigefunktion

data Node = Source | Sink deriving (Eq, Ord, Enum, Show)

instance Node.C Node where
   display Source = Format.literal "Dies ist eine Quelle."
   display Sink   = Format.literal "Dies ist eine Senke."

   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault


topo3 :: TD.Topology Node
topo3 = Gr.fromList nodes (makeEdges edges)
  where nodes = [(Sink, TD.AlwaysSink), (Source, TD.AlwaysSource)]
        edges = [(Source, Sink)]

-------------------------------------------------

main :: IO ()
main =
  concurrentlyMany_ $
    Draw.topology topo0 :
    Draw.topology topo1 :
    Draw.topology topo2 :
    Draw.topology topo3 :
    []
