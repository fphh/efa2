{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module EFA.Equation.Absolute (
   module EFA.Equation.Absolute,
   (=.=),
   ) where

import qualified EFA.Example.Utility as Utility

import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.Variable as Var
import qualified EFA.Symbolic.Mixed as Term
import EFA.Equation.System ((=.=))
import EFA.Equation.Result(Result(..))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Utility (Pointed, point)

import qualified UniqueLogic.ST.Expression as Expr
import qualified UniqueLogic.ST.System as Sys

import Control.Applicative (liftA, liftA2)
import Data.Monoid ((<>))


type EquationSystem = EqGen.EquationSystem Env.Absolute

type Expression node s a v x = EqGen.Expression Env.Absolute node s a v x


solve ::
   (Eq a, Arith.Product a, a ~ Arith.Scalar v,
    Eq v, Arith.Product v, Arith.Integrate v,
    Node.C node) =>
   TD.SequFlowGraph node ->
   (forall s. EquationSystem node s a v) ->
   Env.Complete node (Env.Absolute (Result a)) (Env.Absolute (Result v))
solve = EqGen.solve


constant :: x -> Expression node s a v x
constant = EqGen.constant

variableSignal ::
   (Eq v, Arith.Sum v, Env.AccessSignalMap idx, Ord (idx node)) =>
   idx node -> Expression node s a v v
variableSignal = EqGen.variableSignal . Idx.absolute

variableScalar ::
   (Eq a, Arith.Sum a, Env.AccessScalarMap idx, Ord (idx node)) =>
   idx node -> Expression node s a v a
variableScalar = EqGen.variableScalar . Idx.absolute


liftF ::
  (Arith.Sum y) =>
  (x -> y) ->
  Expression node s a v x ->
  Expression node s a v y
liftF = liftA . Expr.fromRule2 . Sys.assignment2 ""

liftF2 ::
  (Arith.Sum z) =>
  (x -> y -> z) ->
  Expression node s a v x ->
  Expression node s a v y ->
  Expression node s a v z
liftF2 = liftA2 . Expr.fromRule3 . Sys.assignment3 ""



type SignalTerm term node = Utility.SignalTerm Env.Absolute term node
type ScalarTerm term node = Utility.ScalarTerm Env.Absolute term node
type ScalarAtom term node = Utility.ScalarAtom Env.Absolute term node

type
   SymbolicEquationSystem node s term =
      Utility.SymbolicEquationSystem Env.Absolute node s term


givenSignalSymbol ::
   (Eq (term (Idx.Record Idx.Absolute (Var.Signal node))),
    Arith.Sum (term (Idx.Record Idx.Absolute (Var.Signal node))),
    Pointed term, Ord (idx node),
    Var.Index idx, Var.Type idx ~ Var.Signal,
    Env.AccessSignalMap idx) =>
   idx node ->
   SymbolicEquationSystem node s term
givenSignalSymbol idx =
   idx .= Term.Signal (point (Idx.absolute (Var.index idx)))

givenScalarSymbol ::
   (Eq (term (ScalarAtom term node)),
    Arith.Sum (term (ScalarAtom term node)),
    Pointed term, Ord (idx node),
    Var.Index idx, Var.Type idx ~ Var.Scalar,
    Env.AccessScalarMap idx) =>
   idx node ->
   SymbolicEquationSystem node s term
givenScalarSymbol idx =
   idx #= Term.Scalar (point (Term.ScalarVariable (Idx.absolute (Var.index idx))))


infixr 6 =<>, #=<>

(=<>) ::
   (Eq (term (Idx.Record Idx.Absolute (Var.Signal node))),
    Arith.Sum (term (Idx.Record Idx.Absolute (Var.Signal node))),
    Pointed term, Ord (idx node),
    Var.Index idx, Var.Type idx ~ Var.Signal,
    Env.AccessSignalMap idx) =>
   idx node ->
   SymbolicEquationSystem node s term ->
   SymbolicEquationSystem node s term
idx =<> eqsys = givenSignalSymbol idx <> eqsys

(#=<>) ::
   (Eq (term (ScalarAtom term node)),
    Arith.Sum (term (ScalarAtom term node)),
    Pointed term, Ord (idx node),
    Var.Index idx, Var.Type idx ~ Var.Scalar,
    Env.AccessScalarMap idx) =>
   idx node ->
   SymbolicEquationSystem node s term ->
   SymbolicEquationSystem node s term
idx #=<> eqsys = givenScalarSymbol idx <> eqsys


infix 0 .=, #=

(.=) ::
   (Eq v, Arith.Sum v, Env.AccessSignalMap idx, Ord (idx node)) =>
   idx node -> v ->
   EquationSystem node s a v
evar .= val  =  variableSignal evar =.= EqGen.constant val

(#=) ::
   (Eq a, Arith.Sum a, Env.AccessScalarMap idx, Ord (idx node)) =>
   idx node -> a ->
   EquationSystem node s a v
evar #= val  =  variableScalar evar =.= EqGen.constant val
