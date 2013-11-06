module EFA.Flow.Absolute (
   Expression, ExpressionIgnore,
   VariableSystem, VariableSystemIgnore,
   liftF, liftF2,
   ) where

import EFA.Flow.EquationSystem (Expression, VariableSystem)

import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Arithmetic as Arith

import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.System as Sys

import Control.Applicative (liftA, liftA2)

import qualified Prelude as P
import Prelude hiding (sqrt)


type ExpressionIgnore var s x = Expression Verify.Ignore var s x

type VariableSystemIgnore var s = VariableSystem Verify.Ignore var s


liftF ::
   (Arith.Sum y, Sys.Value mode y) =>
   (x -> y) ->
   Expression mode var s x ->
   Expression mode var s y
liftF = liftA . Expr.fromRule2 . Sys.assignment2

liftF2 ::
   (Arith.Sum z, Sys.Value mode z) =>
   (x -> y -> z) ->
   Expression mode var s x ->
   Expression mode var s y ->
   Expression mode var s z
liftF2 = liftA2 . Expr.fromRule3 . Sys.assignment3
