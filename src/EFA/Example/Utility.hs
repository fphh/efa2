module EFA.Example.Utility where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr
import EFA.Graph.Topology.StateAnalysis (bruteForce)

import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.System as EqGen
import EFA.Equation.System ((=.=))
import EFA.Equation.Variable (MkIdxC, MkVarC, mkVar)
import qualified EFA.Equation.Variable as Var
import Data.Monoid ((<>))

import EFA.Signal.SequenceData (SequData(SequData))



{-
makeNode :: Int -> Idx.Node
makeNode = Idx.Node

makeNodes :: [(Int, TD.NodeType)] -> [Gr.LNode Idx.Node TD.NodeType]
makeNodes ns = map f ns
  where f (n, ty) = (makeNode n, ty)
-}

makeEdges :: [(node, node)] -> [Gr.LEdge node ()]
makeEdges = map (\(a, b) -> (Gr.Edge a b, ()))

{-
makeSimpleEdges :: [(Int, Int)] -> [Gr.LEdge Idx.Node ()]
makeSimpleEdges es = map f es
  where f (a, b) = (Gr.Edge (Idx.Node a) (Idx.Node b), ())
-}

constructSeqTopo ::
  (Ord node) =>
  TD.Topology node -> [Int] -> TD.SequFlowGraph node
constructSeqTopo topo =
  Flow.mkSequenceTopology .
  Flow.genSectionTopology .
  fmap (bruteForce topo !!) .
  SequData


recAbs :: Idx.Absolute
recAbs = Idx.Absolute


selfAssign ::
  (Eq (term (Var.Index rec node)), Ord (t rec node), MkVarC term, MkIdxC t,
   Env.AccessMap t) =>
  t rec node ->
  EqGen.EquationSystem rec node s (term (Var.Index rec node))
selfAssign idx =
   EqGen.getVar idx .= mkVar idx

infixr 6 =<>
(=<>) ::
  ( Eq (term (Var.Index rec node)), Ord (t rec node), Env.AccessMap t,
    MkVarC term, MkIdxC t) =>
  t rec node ->
  EqGen.EquationSystem rec node s (term (Var.Index rec node)) ->
  EqGen.EquationSystem rec node s (term (Var.Index rec node))
idx =<> eqsys = selfAssign idx <> eqsys


edgeVar ::
   (Idx.SecNode node -> Idx.SecNode node -> idx) ->
   Idx.Section -> node -> node -> idx
edgeVar idx sec x y =
   idx
      (Idx.SecNode sec x)
      (Idx.SecNode sec y)

interVar ::
   (Idx.SecNode node -> Idx.SecNode node -> idx) ->
   Idx.Section -> Idx.Section -> node -> idx
interVar idx sec0 sec1 x =
   idx
      (Idx.SecNode sec0 x)
      (Idx.SecNode sec1 x)


infix 0 .=

(.=) ::
  (Eq a) =>
  EqGen.ExprWithVars rec node s a -> a -> EqGen.EquationSystem rec node s a
evar .= val  =  evar =.= EqGen.constToExprSys val
