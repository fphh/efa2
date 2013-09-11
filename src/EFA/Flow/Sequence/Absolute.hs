{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module EFA.Flow.Sequence.Absolute (
   module EFA.Flow.Sequence.Absolute,
   (=.=),
   ) where

import qualified EFA.Application.Symbolic as Symbolic

import qualified EFA.Symbolic.Variable as SymVar

import qualified EFA.Flow.Sequence.EquationSystem as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import EFA.Flow.Sequence.EquationSystem ((=.=))

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result(Result(..))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node

import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.System as Sys

import qualified Control.Monad.Exception.Synchronous as ME

import Control.Applicative (liftA, liftA2)
import Data.Tuple.HT (mapFst)


type
   EquationSystem mode node s a v =
      EqSys.EquationSystem mode Record.Absolute node s a v

type
   Expression mode node s a v x =
      EqSys.Expression mode Record.Absolute node s a v x


solve ::
   (Eq a, Arith.Constant a, a ~ Arith.Scalar v,
    Eq v, Arith.Product v, Arith.Integrate v,
    Node.C node) =>
   SeqFlow.Graph node (Result a) (Result v) ->
   (forall s. EquationSystem Verify.Ignore node s a v) ->
   SeqFlow.Graph node (Result a) (Result v)
solve graph sys =
   SeqFlow.mapGraph Record.unAbsolute Record.unAbsolute $
   EqSys.solve (SeqFlow.mapGraph Record.Absolute Record.Absolute graph) sys

solveTracked ::
   (Verify.GlobalVar (Verify.Track output) a Idx.Absolute Var.ForNodeSectionScalar node,
    Arith.Constant a, a ~ Arith.Scalar v,
    Verify.GlobalVar (Verify.Track output) v Idx.Absolute Var.InSectionSignal node,
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
   (Sys.Value mode x, x ~ Env.Element idx a v,
    SeqFlow.Lookup idx, Node.C node) =>
   idx node -> Expression mode node s a v x
variable = EqSys.variable . Idx.absolute


liftF ::
   (Arith.Sum y, Sys.Value mode y) =>
   (x -> y) ->
   Expression mode node s a v x ->
   Expression mode node s a v y
liftF = liftA . Expr.fromRule2 . Sys.assignment2

liftF2 ::
   (Arith.Sum z, Sys.Value mode z) =>
   (x -> y -> z) ->
   Expression mode node s a v x ->
   Expression mode node s a v y ->
   Expression mode node s a v z
liftF2 = liftA2 . Expr.fromRule3 . Sys.assignment3


type SignalTerm term node = Symbolic.SignalTerm Idx.Absolute term node
type ScalarTerm term node = Symbolic.ScalarTerm Idx.Absolute term node
type ScalarAtom term node = Symbolic.ScalarAtom Idx.Absolute term node

type VarTerm var term node = SymVar.VarTerm var Idx.Absolute term node


infix 0 .=, =%%=

(.=) ::
   (Sys.Value mode x, x ~ Env.Element idx a v,
    SeqFlow.Lookup idx, Node.C node) =>
   idx node -> x -> EquationSystem mode node s a v
evar .= val  =  variable evar =.= constant val


(=%%=) ::
   (Sys.Value mode x, x ~ Env.Element idx a v,
    SeqFlow.Lookup idx, Node.C node) =>
   idx node -> idx node -> EquationSystem mode node s a v
(=%%=) = (EqSys.=%%=)
