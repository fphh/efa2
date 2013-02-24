{-# LANGUAGE Rank2Types #-}
module EFA.Equation.Absolute where

import qualified EFA.Example.Utility as Utility

import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.Variable as Var
import EFA.Equation.System ((=.=))
import EFA.Equation.Variable (MkIdxC, MkVarC)
import EFA.Equation.Result(Result(..))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD

import Data.Monoid ((<>))

import qualified Prelude as P
import Prelude hiding (sqrt)


type EquationSystem = EqGen.EquationSystem Idx.Absolute

type Expression node s a x = EqGen.ExprWithVars Idx.Absolute node s a x


solve ::
   (Eq a, Fractional a, Node.C node) =>
   (forall s. EquationSystem node s a) ->
   TD.SequFlowGraph node -> Env.Env Idx.Absolute node (Result a)
solve = EqGen.solve


constant :: x -> Expression node s a x
constant = EqGen.constant

getVar ::
   (Env.AccessMap idx, Ord (idx node)) =>
   idx node -> Expression node s a a
getVar = EqGen.getVar . Idx.absolute




type Term term node = Utility.Term term Idx.Absolute node

givenSymbol ::
   (Eq (Term term node), MkVarC term,
    Ord (t node), MkIdxC t, Env.AccessMap t) =>
   t node ->
   EquationSystem node s (Term term node)
givenSymbol idx =
   idx .=
   Var.mkVarCore (Idx.absolute (Var.mkIdx idx))


infixr 6 =<>

(=<>) ::
   (Eq (Term term node), MkVarC term,
    Ord (t node), MkIdxC t, Env.AccessMap t) =>
   t node ->
   EquationSystem node s (Term term node) ->
   EquationSystem node s (Term term node)
idx =<> eqsys = givenSymbol idx <> eqsys


infix 0 .=

(.=) ::
   (Eq a, Env.AccessMap idx, Ord (idx node)) =>
   idx node -> a ->
   EquationSystem node s a
evar .= val  =  getVar evar =.= EqGen.constant val
