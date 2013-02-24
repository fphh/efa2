module EFA.Example.Utility where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr
import EFA.Graph.Topology.StateAnalysis (bruteForce)

import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.System as EqGen
import EFA.Equation.System ((=.=))
import EFA.Equation.Variable (MkIdxC, MkVarC, mkVarCore, mkIdx)
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


type Term term rec node = term (Idx.Record rec (Var.Index node))

givenSymbol ::
  (Eq (Term term rec node), MkVarC term,
   Ord (t node), MkIdxC t, Env.AccessMap t,
   EqGen.Record rec) =>
  Idx.Record rec (t node) ->
  EqGen.EquationSystem rec node s (Term term rec node)
givenSymbol idx =
   EqGen.getVar idx .= mkVarCore (fmap mkIdx idx)

infixr 6 =<>
(=<>) ::
  (Eq (Term term rec node), MkVarC term,
   Ord (t node), MkIdxC t, Env.AccessMap t,
   EqGen.Record rec) =>
  Idx.Record rec (t node) ->
  EqGen.EquationSystem rec node s (Term term rec node) ->
  EqGen.EquationSystem rec node s (Term term rec node)
idx =<> eqsys = givenSymbol idx <> eqsys


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
  (Eq x) =>
  EqGen.ExprWithVars rec node s a x -> x ->
  EqGen.EquationSystem rec node s a
evar .= val  =  evar =.= EqGen.constant val
