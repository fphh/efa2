{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module EFA.Example.Absolute (
   module EFA.Example.Absolute,
   (=.=),
   ) where

import qualified EFA.Example.Utility as Utility

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Variable as Var
import EFA.Equation.System ((=.=))
import EFA.Equation.Result(Result(..))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Utility (Pointed)

import qualified UniqueLogic.ST.Expression as Expr
import qualified UniqueLogic.ST.System as Sys

import Control.Applicative (liftA, liftA2)
import Data.Monoid ((<>))


type EquationSystem = EqGen.EquationSystem Record.Absolute

type Expression node s a v x = EqGen.Expression Record.Absolute node s a v x


solve ::
   (Eq a, Arith.Product a, a ~ Arith.Scalar v,
    Eq v, Arith.Product v, Arith.Integrate v,
    Node.C node) =>
   TD.SequFlowGraph node ->
   (forall s. EquationSystem node s a v) ->
   Env.Complete node (Record.Absolute (Result a)) (Record.Absolute (Result v))
solve = EqGen.solve


constant :: x -> Expression node s a v x
constant = EqGen.constant

variable ::
   (Eq x, Arith.Sum x,
    EqGen.Element idx Record.Absolute s a v ~ EqGen.VariableRecord Record.Absolute s x,
    Env.AccessMap idx, Ord (idx node)) =>
   idx node -> Expression node s a v x
variable = EqGen.variable . Idx.absolute


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



type SignalTerm term node = Utility.SignalTerm Record.Absolute term node
type ScalarTerm term node = Utility.ScalarTerm Record.Absolute term node
type ScalarAtom term node = Utility.ScalarAtom Record.Absolute term node

type VarTerm var term node = Utility.VarTerm var Idx.Absolute term node

type
   SymbolicEquationSystem node s term =
      Utility.SymbolicEquationSystem Record.Absolute node s term

symbol ::
   (Utility.Symbol var, Pointed term) =>
   var node -> VarTerm var term node
symbol = Utility.symbol . Idx.absolute

givenSymbol ::
  (t ~ VarTerm var term node,
   Eq t, Arith.Sum t,
   EqGen.Element idx Record.Absolute s
      (ScalarTerm term node) (SignalTerm term node)
     ~ EqGen.VariableRecord Record.Absolute s t,
   Ord (idx node), Pointed term,
   Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>
  idx node ->
  SymbolicEquationSystem node s term
givenSymbol idx =
   idx .= symbol (Var.index idx)


infixr 6 =<>

(=<>) ::
  (t ~ VarTerm var term node,
   Eq t, Arith.Sum t,
   EqGen.Element idx Record.Absolute s
      (ScalarTerm term node) (SignalTerm term node)
     ~ EqGen.VariableRecord Record.Absolute s t,
   Ord (idx node), Pointed term,
   Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>
   idx node ->
   SymbolicEquationSystem node s term ->
   SymbolicEquationSystem node s term
idx =<> eqsys = givenSymbol idx <> eqsys


infix 0 .=

(.=) ::
  (Eq x, Arith.Sum x,
   EqGen.Element idx Record.Absolute s a v
      ~ EqGen.VariableRecord Record.Absolute s x,
   Env.AccessMap idx, Ord (idx node)) =>
   idx node -> x ->
   EquationSystem node s a v
evar .= val  =  variable evar =.= EqGen.constant val
