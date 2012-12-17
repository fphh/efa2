
module EFA2.Example.ExampleHelper where

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Topology.EfaGraph as Gr
import qualified EFA2.Topology.TopologyData as TD
import qualified EFA2.Interpreter.Env as Env
import EFA2.Topology.EquationGenerator (makeVar)
import EFA2.Solver.Equation (MkIdxC, mkVar)

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


rec :: Idx.Record
rec = Idx.Record Idx.Absolute

var ::
   (MkIdxC idx) =>
   (Idx.Record -> a -> idx) -> a -> Env.Index
var idx x = mkVar (idx rec x)

edgeVar ::
   MkIdxC idx =>
   (Idx.Record -> Idx.SecNode -> Idx.SecNode -> idx) ->
   Idx.Section -> Idx.Node -> Idx.Node -> Env.Index
edgeVar idx sec x y =
   makeVar idx
      (Idx.SecNode sec x)
      (Idx.SecNode sec y)

interVar ::
   MkIdxC idx =>
   (Idx.Record -> Idx.SecNode -> Idx.SecNode -> idx) ->
   Idx.Section -> Idx.Section -> Idx.Node -> Env.Index
interVar idx sec0 sec1 x =
   makeVar idx
      (Idx.SecNode sec0 x)
      (Idx.SecNode sec1 x)
