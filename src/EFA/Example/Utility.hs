module EFA.Example.Utility where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr
import EFA.Graph.Topology.StateAnalysis (bruteForce)

import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Variable as Var
import EFA.Equation.System ((=.=), (=%=))
import EFA.Equation.Variable (MkIdxC, MkVarC)
import EFA.Signal.SequenceData (SequData(SequData))

import Data.Monoid ((<>))



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



type Term term recIdx node = term (Idx.Record recIdx (Var.Index node))

givenSymbol ::
  {-
  The Eq constraint is requested by unique-logic but not really needed.
  It is not easily possible to compare the terms for equal meaning
  and it is better not to compare them at all.
  We should remove the Eq constraint as soon as unique-logic allows it.
  -}
  (Eq (Term term recIdx node),
   Fractional (Term term recIdx node),
   EqGen.Record rec,
   Ord (idx node), MkVarC term, MkIdxC idx,
   Env.AccessMap idx, Env.Record recIdx rec) =>
  Idx.Record recIdx (idx node) ->
  EqGen.EquationSystem rec node s (Term term recIdx node)
givenSymbol idx =
   idx .= Var.mkVarCore (fmap Var.mkIdx idx)


infixr 6 =<>
(=<>) ::
  (Eq (Term term recIdx node),
   Fractional (Term term recIdx node),
   EqGen.Record rec,
   Ord (idx node), MkVarC term, MkIdxC idx,
   Env.AccessMap idx, Env.Record recIdx rec) =>
  Idx.Record recIdx (idx node) ->
  EqGen.EquationSystem rec node s (Term term recIdx node) ->
  EqGen.EquationSystem rec node s (Term term recIdx node)
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


infix 0 .=, %=

(.=) ::
  (Eq a, Num a, EqGen.Record rec, Env.Record recIdx rec,
   Env.AccessMap idx, Ord (idx node)) =>
  Idx.Record recIdx (idx node) -> a ->
  EqGen.EquationSystem rec node s a
evar .= val  =  EqGen.getVar evar =.= EqGen.constant val

(%=) ::
  (Eq x, Num x, EqGen.Record rec, Env.Record recIdx rec,
   Env.AccessMap idx, Ord (idx node)) =>
  idx node -> rec x ->
  EqGen.EquationSystem rec node s x
evar %= val  =  EqGen.getRecordVar evar =%= EqGen.constantRecord val
