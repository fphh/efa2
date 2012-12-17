
module EFA2.Example.ExampleHelper where


import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Topology.EfaGraph as Gr
import qualified EFA2.Topology.TopologyData as TD

makeNode :: Int -> Idx.Node
makeNode = Idx.Node

makeNodes :: [(Int, TD.NodeType)] -> [Gr.LNode Idx.Node TD.NodeType]
makeNodes ns = map f ns
  where f (n, ty) = (makeNode n, ty)

makeEdges :: [(Idx.Node, Idx.Node)] -> [Gr.LEdge Idx.Node ()]
makeEdges = map (\(a, b) -> (Gr.Edge a b, ()))

makeSimpleEdges :: [(Int, Int)] -> [Gr.LEdge Idx.Node ()]
makeSimpleEdges es = map f es
  where f (a, b) = (Gr.Edge (Idx.Node a) (Idx.Node b), ())
