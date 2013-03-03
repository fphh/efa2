{-# LANGUAGE TypeFamilies #-}
module EFA.Example.Utility where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr
import EFA.Graph.Topology.StateAnalysis (bruteForce)

import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Variable as Var
import qualified EFA.Symbolic.Mixed as Term
import EFA.Equation.System ((=.=), (=%=))
import EFA.Signal.SequenceData (SequData(SequData))
import EFA.Utility (Pointed, point)

import qualified EFA.Equation.Arithmetic as Arith

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


type
   SignalTerm rec term node =
      Term.Signal term
         (Env.RecordIndexed rec (Var.Scalar node))
         (Env.RecordIndexed rec (Var.Signal node))

type
   ScalarTerm rec term node =
      Term.Scalar term
         (Env.RecordIndexed rec (Var.Scalar node))
         (Env.RecordIndexed rec (Var.Signal node))

type
   ScalarAtom rec term node =
      Term.ScalarAtom
         term
         (Env.RecordIndexed rec (Var.Scalar node))
         (Env.RecordIndexed rec (Var.Signal node))

type
   SymbolicEquationSystem rec node s term =
      EqGen.EquationSystem rec node s
         (ScalarTerm rec term node)
         (SignalTerm rec term node)

givenSignalSymbol ::
  {-
  The Eq constraint is requested by unique-logic but not really needed.
  It is not easily possible to compare the terms for equal meaning
  and it is better not to compare them at all.
  We should remove the Eq constraint as soon as unique-logic allows it.
  -}
  (Eq (term (Env.RecordIndexed rec (Var.Signal node))),
   Arith.Sum (term (Env.RecordIndexed rec (Var.Signal node))),
   EqGen.Record rec,
   Ord (idx node), Pointed term,
   Var.Index idx, Var.Type idx ~ Var.Signal,
   Env.AccessSignalMap idx) =>
  Env.RecordIndexed rec (idx node) ->
  SymbolicEquationSystem rec node s term
givenSignalSymbol idx =
   idx .= Term.Signal (point (fmap Var.index idx))

givenScalarSymbol ::
  (Eq (term (ScalarAtom rec term node)),
   Arith.Sum (term (ScalarAtom rec term node)),
   EqGen.Record rec,
   Ord (idx node), Pointed term,
   Var.Index idx, Var.Type idx ~ Var.Scalar,
   Env.AccessScalarMap idx) =>
  Env.RecordIndexed rec (idx node) ->
  SymbolicEquationSystem rec node s term
givenScalarSymbol idx =
   idx #= Term.Scalar (point (Term.ScalarVariable (fmap Var.index idx)))


infixr 6 =<>, #=<>

(=<>) ::
  (Eq (term (Env.RecordIndexed rec (Var.Signal node))),
   Arith.Sum (term (Env.RecordIndexed rec (Var.Signal node))),
   EqGen.Record rec,
   Ord (idx node), Pointed term,
   Var.Index idx, Var.Type idx ~ Var.Signal,
   Env.AccessSignalMap idx) =>
  Env.RecordIndexed rec (idx node) ->
  SymbolicEquationSystem rec node s term ->
  SymbolicEquationSystem rec node s term
idx =<> eqsys = givenSignalSymbol idx <> eqsys

(#=<>) ::
  (Eq (term (ScalarAtom rec term node)),
   Arith.Sum (term (ScalarAtom rec term node)),
   EqGen.Record rec,
   Ord (idx node), Pointed term,
   Var.Index idx, Var.Type idx ~ Var.Scalar,
   Env.AccessScalarMap idx) =>
  Env.RecordIndexed rec (idx node) ->
  SymbolicEquationSystem rec node s term ->
  SymbolicEquationSystem rec node s term
idx #=<> eqsys = givenScalarSymbol idx <> eqsys


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


infix 0 .=, %=, #=

(.=) ::
  (Eq v, Arith.Sum v, EqGen.Record rec,
   Env.AccessSignalMap idx, Ord (idx node)) =>
  Env.RecordIndexed rec (idx node) -> v ->
  EqGen.EquationSystem rec node s a v
evar .= val  =  EqGen.variableSignal evar =.= EqGen.constant val

(#=) ::
  (Eq a, Arith.Sum a, EqGen.Record rec,
   Env.AccessScalarMap idx, Ord (idx node)) =>
  Env.RecordIndexed rec (idx node) -> a ->
  EqGen.EquationSystem rec node s a v
evar #= val  =  EqGen.variableScalar evar =.= EqGen.constant val

(%=) ::
  (Eq v, Arith.Sum v, EqGen.Record rec,
   Env.AccessSignalMap idx, Ord (idx node)) =>
  idx node -> rec v ->
  EqGen.EquationSystem rec node s a v
evar %= val  =  EqGen.variableSignalRecord evar =%= EqGen.constantRecord val
