{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module EFA.Flow.State.Absolute (
   module EFA.Flow.State.Absolute,
   EqSys.optionsDefault, EqSys.optionsBase,
   EqSys.equalInOutSums, EqSys.independentInOutSums,
   EqSys.CoupleSums, EqSys.integrateStInOutSums, EqSys.equalStInOutSums,
   Abs.liftF, Abs.liftF2,
   (=.=),
   ) where

import qualified EFA.Flow.State.EquationSystem as EqSys
import qualified EFA.Flow.State.Quantity as StateFlow
import qualified EFA.Flow.SequenceState.Variable as Var
import qualified EFA.Flow.Absolute as Abs
import EFA.Flow.State.EquationSystem ((=.=))

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result(Result)

import qualified EFA.Graph.Topology.Node as Node

import qualified UniqueLogic.ST.TF.System as Sys

import qualified Control.Monad.Exception.Synchronous as ME

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
    Verify.GlobalVar mode a RecIdx.Absolute Var.ForStorageStateScalar node,
    Verify.GlobalVar mode v RecIdx.Absolute Var.InStateSignal node) =>
   (StateFlow.Graph node
       (Expression mode node s a v a)
       (Expression mode node s a v v) ->
    EquationSystem mode node s a v) ->
   EquationSystem mode node s a v
withExpressionGraph f =
   EqSys.withExpressionGraph $
      f .
      StateFlow.mapGraph
         (fmap (Record.unAbsolute . EqSys.unwrap))
         (fmap (Record.unAbsolute . EqSys.unwrap))


solve ::
   (Eq a, Arith.Constant a, Arith.ZeroTestable a, Node.C node) =>
   StateFlow.Graph node (Result a) (Result a) ->
   (forall s. EquationSystem Verify.Ignore node s a a) ->
   StateFlow.Graph node (Result a) (Result a)
solve graph sys =
   StateFlow.mapGraph Record.unAbsolute Record.unAbsolute $
   EqSys.solve (StateFlow.mapGraph Record.Absolute Record.Absolute graph) sys

solveOpts ::
   (Arith.Product a, Arith.ZeroTestable a,
    Arith.Product v, Arith.ZeroTestable v,
    Node.C node) =>
   (forall s. Options Verify.Ignore s a v) ->
   StateFlow.Graph node (Result a) (Result v) ->
   (forall s. EquationSystem Verify.Ignore node s a v) ->
   StateFlow.Graph node (Result a) (Result v)
solveOpts opts graph sys =
   StateFlow.mapGraph Record.unAbsolute Record.unAbsolute $
   EqSys.solveOpts opts
      (StateFlow.mapGraph Record.Absolute Record.Absolute graph) sys

solveTracked ::
   (Verify.GlobalVar (Verify.Track output) a RecIdx.Absolute Var.ForStorageStateScalar node,
    Verify.GlobalVar (Verify.Track output) a RecIdx.Absolute Var.InStateSignal node,
    Arith.Constant a, Arith.ZeroTestable a, Node.C node) =>
   StateFlow.Graph node (Result a) (Result a) ->
   (forall s. EquationSystem (Verify.Track output) node s a a) ->
   (ME.Exceptional
      (Verify.Exception output)
      (StateFlow.Graph node (Result a) (Result a)),
    Verify.Assigns output)
solveTracked graph sys =
   mapFst (fmap (StateFlow.mapGraph Record.unAbsolute Record.unAbsolute)) $
   EqSys.solveTracked (StateFlow.mapGraph Record.Absolute Record.Absolute graph) sys


constant ::
   (Sys.Value mode x) =>
   x -> Expression mode node s a v x
constant = EqSys.constant

variable ::
   (Sys.Value mode x, x ~ StateFlow.Element idx a v,
    StateFlow.Lookup idx, Node.C node) =>
   idx node -> Expression mode node s a v x
variable = EqSys.variable . RecIdx.absolute


infix 0 .=, =%%=

(.=) ::
   (Sys.Value mode x, x ~ StateFlow.Element idx a v,
    StateFlow.Lookup idx, Node.C node) =>
   idx node -> x -> EquationSystem mode node s a v
evar .= val  =  variable evar =.= constant val


(=%%=) ::
   (Sys.Value mode x, x ~ StateFlow.Element idx a v,
    StateFlow.Lookup idx, Node.C node) =>
   idx node -> idx node -> EquationSystem mode node s a v
(=%%=) = (EqSys.=%%=)
