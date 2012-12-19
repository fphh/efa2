
module EFA2.Example.ExampleHelper where

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Topology.EfaGraph as Gr
import qualified EFA2.Topology.TopologyData as TD
import qualified EFA2.Interpreter.Env as Env
import qualified EFA2.Topology.EquationGenerator as EqGen
import EFA2.Topology.EquationGenerator ((=.=))
import EFA2.Solver.Equation (Term(Atom), EqTerm, MkIdxC(mkIdx))
import Data.Monoid ((<>))


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


selfAssign ::
   (MkIdxC idx, Env.AccessMap idx) =>
   idx -> EqGen.EquationSystem s EqTerm
selfAssign idx =
   EqGen.getVar idx .= Atom (mkIdx idx)

infixr 6 =<>

(=<>) ::
   (MkIdxC idx, Env.AccessMap idx) =>
   idx ->
   EqGen.EquationSystem s EqTerm ->
   EqGen.EquationSystem s EqTerm
idx =<> eqsys = selfAssign idx <> eqsys


rec :: Idx.Record
rec = Idx.Record Idx.Absolute

edgeVar ::
   (Idx.SecNode -> Idx.SecNode -> idx) ->
   Idx.Section -> Idx.Node -> Idx.Node -> idx
edgeVar idx sec x y =
   idx
      (Idx.SecNode sec x)
      (Idx.SecNode sec y)

interVar ::
   (Idx.SecNode -> Idx.SecNode -> idx) ->
   Idx.Section -> Idx.Section -> Idx.Node -> idx
interVar idx sec0 sec1 x =
   idx
      (Idx.SecNode sec0 x)
      (Idx.SecNode sec1 x)


infix 0 .=

(.=) :: Eq a => EqGen.ExprWithVars s a -> a -> EqGen.EquationSystem s a
evar .= val  =  evar =.= EqGen.constToExprSys val
