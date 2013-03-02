{-# LANGUAGE Rank2Types #-}
module EFA.Equation.Absolute where

import qualified EFA.Example.Utility as Utility

import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.Variable as Var
import EFA.Equation.System ((=.=))
import EFA.Equation.Variable (MkIdxC)
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

type Expression node s a x = EqGen.Expression Env.Absolute node s a x


solve ::
   (Eq a, Arith.Product a, Node.C node) =>
   (forall s. EquationSystem node s a) ->
   TD.SequFlowGraph node -> Env.Env node (Env.Absolute (Result a))
solve = EqGen.solve


constant :: x -> Expression node s a x
constant = EqGen.constant

getVar ::
   (Eq a, Arith.Sum a, Env.AccessMap idx, Ord (idx node)) =>
   idx node -> Expression node s a a
getVar = EqGen.getVar . Idx.absolute


liftF ::
  (Arith.Sum y) =>
  (x -> y) ->
  Expression node s a x ->
  Expression node s a y
liftF = liftA . Expr.fromRule2 . Sys.assignment2 ""

liftF2 ::
  (Arith.Sum z) =>
  (x -> y -> z) ->
  Expression node s a x ->
  Expression node s a y ->
  Expression node s a z
liftF2 = liftA2 . Expr.fromRule3 . Sys.assignment3 ""



type Term term node = Utility.Term term Idx.Absolute node

givenSymbol ::
   (Eq (Term term node), Arith.Sum (Term term node), Pointed term,
    Ord (t node), MkIdxC t, Env.AccessMap t) =>
   t node ->
   EquationSystem node s (Term term node)
givenSymbol idx =
   idx .=
   point (Idx.absolute (Var.mkIdx idx))


infixr 6 =<>

(=<>) ::
   (Eq (Term term node), Arith.Sum (Term term node), Pointed term,
    Ord (t node), MkIdxC t, Env.AccessMap t) =>
   t node ->
   EquationSystem node s (Term term node) ->
   EquationSystem node s (Term term node)
idx =<> eqsys = givenSymbol idx <> eqsys


infix 0 .=

(.=) ::
   (Eq a, Arith.Sum a, Env.AccessMap idx, Ord (idx node)) =>
   idx node -> a ->
   EquationSystem node s a
evar .= val  =  getVar evar =.= EqGen.constant val
