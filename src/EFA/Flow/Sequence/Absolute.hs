{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module EFA.Flow.Sequence.Absolute (
   module EFA.Flow.Sequence.Absolute,
   EqSys.optionsDefault,
   EqSys.equalInOutSums, EqSys.independentInOutSums,
   EqSys.integrateStInOutSums, EqSys.equalStInOutSums,
   Abs.liftF, Abs.liftF2,
   (=.=),
   ) where

import qualified EFA.Symbolic.Variable as SymVar

import qualified EFA.Flow.Sequence.Symbolic as Symbolic
import qualified EFA.Flow.Sequence.EquationSystem as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Absolute as Abs
import EFA.Flow.Sequence.EquationSystem ((=.=))

import qualified EFA.Flow.SequenceState.Variable as Var
import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result)

import qualified EFA.Graph.Topology.Node as Node

import EFA.Utility (Pointed)

import qualified UniqueLogic.ST.TF.System as Sys

import qualified Control.Monad.Exception.Synchronous as ME

import Data.Monoid ((<>))
import Data.Tuple.HT (mapFst)


type
   EquationSystem mode node s a v =
      EqSys.EquationSystem mode Record.Absolute node s a v

type
   Expression mode node s a v x =
      EqSys.Expression mode Record.Absolute node s a v x

type
   Options mode s a v =
      EqSys.Options mode Record.Absolute s a v


type
   EquationSystemIgnore node s a v =
      EqSys.EquationSystem Verify.Ignore Record.Absolute node s a v

type
   ExpressionIgnore node s a v x =
      EqSys.Expression Verify.Ignore Record.Absolute node s a v x



withExpressionGraph ::
   (Node.C node,
    Verify.GlobalVar mode a RecIdx.Absolute Var.ForStorageSectionScalar node,
    Verify.GlobalVar mode v RecIdx.Absolute Var.InSectionSignal node) =>
   (SeqFlow.Graph node
       (Expression mode node s a v a)
       (Expression mode node s a v v) ->
    EquationSystem mode node s a v) ->
   EquationSystem mode node s a v
withExpressionGraph f =
   EqSys.withExpressionGraph $
      f .
      SeqFlow.mapGraph
         (fmap (Record.unAbsolute . EqSys.unwrap))
         (fmap (Record.unAbsolute . EqSys.unwrap))


solve ::
   (Arith.Constant a, a ~ Arith.Scalar v,
    Arith.Product v, Arith.Integrate v,
    Node.C node) =>
   SeqFlow.Graph node (Result a) (Result v) ->
   (forall s. EquationSystem Verify.Ignore node s a v) ->
   SeqFlow.Graph node (Result a) (Result v)
solve graph sys =
   SeqFlow.mapGraph Record.unAbsolute Record.unAbsolute $
   EqSys.solve (SeqFlow.mapGraph Record.Absolute Record.Absolute graph) sys

solveOpts ::
   (Arith.Constant a, a ~ Arith.Scalar v,
    Arith.Product v, Arith.Integrate v,
    Node.C node) =>
   (forall s. Options Verify.Ignore s a v) ->
   SeqFlow.Graph node (Result a) (Result v) ->
   (forall s. EquationSystem Verify.Ignore node s a v) ->
   SeqFlow.Graph node (Result a) (Result v)
solveOpts opts graph sys =
   SeqFlow.mapGraph Record.unAbsolute Record.unAbsolute $
   EqSys.solveOpts opts
      (SeqFlow.mapGraph Record.Absolute Record.Absolute graph) sys

solveTracked ::
   (Verify.GlobalVar (Verify.Track output) a RecIdx.Absolute Var.ForStorageSectionScalar node,
    Arith.Constant a, a ~ Arith.Scalar v,
    Verify.GlobalVar (Verify.Track output) v RecIdx.Absolute Var.InSectionSignal node,
    Arith.Product v, Arith.Integrate v, Node.C node) =>
   SeqFlow.Graph node (Result a) (Result v) ->
   (forall s. EquationSystem (Verify.Track output) node s a v) ->
   (ME.Exceptional
      (Verify.Exception output)
      (SeqFlow.Graph node (Result a) (Result v)),
    Verify.Assigns output)
solveTracked graph sys =
   mapFst (fmap (SeqFlow.mapGraph Record.unAbsolute Record.unAbsolute)) $
   EqSys.solveTracked (SeqFlow.mapGraph Record.Absolute Record.Absolute graph) sys


constant ::
   (Sys.Value mode x) =>
   x -> Expression mode node s a v x
constant = EqSys.constant

variable ::
   (Sys.Value mode x, x ~ SeqFlow.Element idx a v,
    SeqFlow.Lookup idx, Node.C node) =>
   idx node -> Expression mode node s a v x
variable = EqSys.variable . RecIdx.absolute


type SignalTerm term node = Symbolic.SignalTerm RecIdx.Absolute term node
type ScalarTerm term node = Symbolic.ScalarTerm RecIdx.Absolute term node
type ScalarAtom term node = Symbolic.ScalarAtom RecIdx.Absolute term node

type VarTerm var term node = SymVar.VarTerm var RecIdx.Absolute term node

type
   SymbolicEquationSystem mode node s term =
      Symbolic.EquationSystem mode Record.Absolute node s term

type
   SymbolicEquationSystemIgnore node s term =
      SymbolicEquationSystem Verify.Ignore node s term

symbol ::
   (SymVar.Symbol var, Pointed term) =>
   var node -> VarTerm var term node
symbol = SymVar.symbol . RecIdx.absolute

givenSymbol ::
   (Sys.Value mode t, t ~ VarTerm var term node,
    t ~ SeqFlow.Element idx (ScalarTerm term node) (SignalTerm term node),
    Pointed term, Var.Type idx ~ var, SymVar.Symbol var,
    SeqFlow.Lookup idx, Node.C node) =>
   idx node ->
   SymbolicEquationSystem mode node s term
givenSymbol idx =
   idx .= symbol (Var.index idx)


infixr 6 =<>

(=<>) ::
   (Sys.Value mode t, t ~ VarTerm var term node,
    t ~ SeqFlow.Element idx (ScalarTerm term node) (SignalTerm term node),
    Pointed term, Var.Type idx ~ var, SymVar.Symbol var,
    SeqFlow.Lookup idx, Node.C node) =>
    idx node ->
    SymbolicEquationSystem mode node s term ->
    SymbolicEquationSystem mode node s term
idx =<> eqsys = givenSymbol idx <> eqsys


infix 0 .=, =%%=

(.=) ::
   (Sys.Value mode x, x ~ SeqFlow.Element idx a v,
    SeqFlow.Lookup idx, Node.C node) =>
   idx node -> x -> EquationSystem mode node s a v
evar .= val  =  variable evar =.= constant val


(=%%=) ::
   (Sys.Value mode x, x ~ SeqFlow.Element idx a v,
    SeqFlow.Lookup idx, Node.C node) =>
   idx node -> idx node -> EquationSystem mode node s a v
(=%%=) = (EqSys.=%%=)
