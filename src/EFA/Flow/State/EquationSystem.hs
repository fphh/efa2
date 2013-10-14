{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module EFA.Flow.State.EquationSystem (
   EquationSystem, Expression, RecordExpression,

   solve, solveOpts, solveTracked,

   SeqStateEqSys.Options, SeqStateEqSys.optionsDefault,
   SeqStateEqSys.equalInOutSums, SeqStateEqSys.independentInOutSums,
   SeqStateEqSys.integrateStInOutSums, SeqStateEqSys.equalStInOutSums,

   constant,
   constantRecord,
   EqSys.liftF, EqSys.liftF2,
   EqSys.sqrt,

   Record, Wrap(Wrap, unwrap),

   (=.=), (.=),
   (=%=), (%=), (=%%=),
   (?=), Result(..),
   variable,
   variableRecord,

   ) where

import qualified EFA.Flow.SequenceState.EquationSystem as SeqStateEqSys
import qualified EFA.Flow.State.Quantity as StateFlow
import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Flow.EquationSystem as EqSys
import qualified EFA.Flow.StorageGraph.EquationSystem as StorageEqSys
import qualified EFA.Flow.StorageGraph as StorageGraph
import qualified EFA.Flow.PartMap as PartMap
import EFA.Flow.StorageGraph (StorageGraph(StorageGraph))
import EFA.Flow.PartMap (PartMap)
import EFA.Flow.Topology.EquationSystem (fromTopology)
import EFA.Flow.EquationSystem
          (constant, constantRecord, join, (=%=), (=.=))

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.SystemRecord as SysRecord
import EFA.Equation.Result(Result)
import EFA.Equation.SystemRecord
          (System(System), Record, Wrap(Wrap, unwrap))
import EFA.Equation.Arithmetic
          (Sum, Product, Constant, Integrate, Scalar)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node

import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.System as Sys

import qualified Data.Accessor.Basic as Accessor

import qualified Control.Monad.Exception.Synchronous as ME
import qualified Control.Monad.Trans.Reader as MR
import qualified Control.Monad.Trans.Class as MT
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Writer (runWriterT)

import Control.Monad.ST (ST, runST)

import Control.Applicative (Applicative, pure, liftA2)

import qualified Data.Map as Map

import Data.Traversable (Traversable, traverse)
import Data.Foldable (foldMap, fold)
import Data.Monoid (Monoid, (<>), mconcat)

import qualified Prelude as P
import Prelude hiding (lookup, init)


type
   Graph mode rec node s a v =
      StateFlow.Graph node
         (SysRecord.Variable mode rec s a)
         (SysRecord.Variable mode rec s v)

type
   Expression mode rec node s a v x =
      EqSys.Expression mode (Graph mode rec node s a v) s x

type
   RecordExpression mode rec node s a v x =
      EqSys.RecordExpression mode (Graph mode rec node s a v) rec s x

type
   EquationSystem mode rec node s a v =
      EqSys.VariableSystem mode (Graph mode rec node s a v) s


infix 0 =%%=, .=, %=, ?=

(=%%=) ::
   (Node.C node, StateFlow.Lookup idx, StateFlow.Element idx a v ~ x,
    Record rec, Sys.Value mode x) =>
   idx node -> idx node ->
   EquationSystem mode rec node s a v
x =%%= y  =  variableRecord x =%= variableRecord y

(.=) ::
   (Node.C node, StateFlow.Lookup idx, StateFlow.Element idx a v ~ x,
    Record rec, Sys.Value mode x) =>
   Record.Indexed rec (idx node) -> x ->
   EquationSystem mode rec node s a v
evar .= val  =  variable evar =.= constant val

(%=) ::
   (Node.C node, StateFlow.Lookup idx, StateFlow.Element idx a v ~ x,
    Record rec, Sys.Value mode x) =>
   idx node -> rec x ->
   EquationSystem mode rec node s a v
evar %= val  =  variableRecord evar =%= constantRecord val

(?=) ::
   (Node.C node, StateFlow.Lookup idx, StateFlow.Element idx a v ~ x,
    Record rec, Sys.Value mode x) =>
   idx node -> rec (Result x) ->
   EquationSystem mode rec node s a v
evar ?= val  =
   join $
   fmap
      (fold .
       liftA2
          (\rx var -> foldMap (\x -> pure var =.= constant x) rx)
          (Wrap val))
      (variableRecord evar)


newtype
   Lookup rec node s a v idx env =
      Lookup {
         getLookup ::
            (StateFlow.Environment idx ~ env) =>
            idx node ->
            StateFlow.Graph node
               (SysRecord.Variable mode rec s a)
               (SysRecord.Variable mode rec s v) ->
            Maybe
               (SysRecord.Variable mode rec s (StateFlow.Element idx a v))
      }

lookup ::
   (StateFlow.Lookup idx, Node.C node) =>
   idx node ->
   StateFlow.Graph node
      (SysRecord.Variable mode rec s a)
      (SysRecord.Variable mode rec s v) ->
   Maybe
      (SysRecord.Variable mode rec s (StateFlow.Element idx a v))
lookup =
   getLookup $
   StateFlow.switchPart
      (Lookup $ StateFlow.lookup)
      (Lookup $ StateFlow.lookup)

checkedLookup ::
   (Node.C node, Var.FormatIndex idx) =>
   String -> (idx node -> t -> Maybe b) -> idx node -> t -> b
checkedLookup name =
   Var.checkedLookup ("EquationSystem." ++ name)


variableRecord ::
   (Node.C node, StateFlow.Lookup idx, StateFlow.Element idx a v ~ x, Record rec) =>
   idx node -> RecordExpression mode rec node s a v x
variableRecord idx =
   EqSys.Context $
   MR.asks
      (Wrap . fmap Expr.fromVariable .
       checkedLookup "variableRecord: unknown variable" lookup idx)

variable ::
   (Node.C node, StateFlow.Lookup idx, StateFlow.Element idx a v ~ x, Record rec) =>
   Record.Indexed rec (idx node) ->
   Expression mode rec node s a v x
variable (Idx.Record recIdx idx) =
   fmap (Accessor.get (Record.access recIdx) . unwrap) $
   variableRecord idx


fromGraph ::
   (Verify.LocalVar mode a, Constant a, a ~ Scalar v,
    Verify.LocalVar mode v, Product v, Integrate v,
    Record rec, Node.C node) =>
   SeqStateEqSys.Options mode rec s a v ->
   StateFlow.Graph node
      (SysRecord.Variable mode rec s a)
      (SysRecord.Variable mode rec s v) ->
   EqSys.System mode s
fromGraph opts gv =
   case
      StateFlow.mapGraph
         (Wrap . fmap Expr.fromVariable)
         (Wrap . fmap Expr.fromVariable) gv of
      g ->
         mconcat $
            foldMap
               (fromTopology (SeqStateEqSys.optTopology opts) .
                FlowTopoPlain.dirSectionFromFlowGraph)
               (StateFlow.states g) :
            fromStorageSequences opts g :
            []

fromStorageSequences ::
   (Verify.LocalVar mode a, ra ~ SysRecord.Expr mode rec s a,
    Verify.LocalVar mode v, rv ~ SysRecord.Expr mode rec s v,
    Constant a, Record rec, Node.C node) =>
   SeqStateEqSys.Options mode rec s a v ->
   StateFlow.Graph node
      (SysRecord.Expr mode rec s a)
      (SysRecord.Expr mode rec s v) ->
   EqSys.System mode s
fromStorageSequences opts g =
   let stoutsum sec node =
          checkedLookup "fromStorageSequences inStorages"
             StateFlow.lookupStOutSum (Idx.ForNode (Idx.StOutSum sec) node) g
       stinsum sec node =
          checkedLookup "fromStorageSequences outStorages"
             StateFlow.lookupStInSum (Idx.ForNode (Idx.StInSum sec) node) g
       f node (StorageGraph partMap edges) =
          connectCarryFlow opts g node partMap
          <>
          StorageGraph.foldInStorages
             (\sec -> StorageEqSys.fromInStorages (stoutsum sec node)) edges
          <>
          StorageGraph.foldOutStorages
             (\sec -> StorageEqSys.fromOutStorages (stinsum sec node)) edges
   in  fold $ Map.mapWithKey f $ StateFlow.storages g

connectCarryFlow ::
   (Verify.LocalVar mode a, ra ~ SysRecord.Expr mode rec s a,
    Verify.LocalVar mode v, rv ~ SysRecord.Expr mode rec s v,
    Record rec, Node.C node) =>
   SeqStateEqSys.Options mode rec s a v ->
   StateFlow.Graph node ra rv ->
   node ->
   PartMap Idx.State ra ->
   EqSys.System mode s
connectCarryFlow opts g node partMap =
   fold $
   Map.mapWithKey
      (\state carrySum ->
         SeqStateEqSys.fromStorageSums opts $
         fmap ((,) carrySum) $
         maybe (error "charge: missing sum") id $
         StateFlow.lookupSums (Idx.stateNode state node) g) $
   PartMap.parts partMap


variables ::
   (Node.C node, Record rec, Sum a, Sum v,
    Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeStateScalar node,
    Verify.GlobalVar mode v (Record.ToIndex rec) Var.InStateSignal node) =>
   StateFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   EqSys.Writer mode s
      (StateFlow.Graph node
         (SysRecord.Variable mode rec s a)
         (SysRecord.Variable mode rec s v))
variables =
   StateFlow.traverseGraph id id
   .
   StateFlow.mapGraphWithVar
      EqSys.globalVariableFromResult
      EqSys.globalVariableFromResult

query ::
   (Node.C node, Record rec) =>
   StateFlow.Graph node
      (SysRecord.Variable mode rec s a)
      (SysRecord.Variable mode rec s v) ->
   ST s
      (StateFlow.Graph node
         (rec (Result a))
         (rec (Result v)))
query =
   StateFlow.traverseGraph
      (traverse (fmap Result.fromMaybe . Sys.query))
      (traverse (fmap Result.fromMaybe . Sys.query))


setup ::
   (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeStateScalar node,
    Verify.GlobalVar mode v (Record.ToIndex rec) Var.InStateSignal node,
    Constant a, a ~ Scalar v,
    Product v, Integrate v,
    Record rec, Node.C node) =>
   SeqStateEqSys.Options mode rec s a v ->
   StateFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   EquationSystem mode rec node s a v ->
   ST s
      (StateFlow.Graph node
         (SysRecord.Variable mode rec s a)
         (SysRecord.Variable mode rec s v),
       Sys.T mode s ())
setup opts gr given = do
   (vars, System eqs) <-
      runWriterT $ do
         vars <- variables gr
         EqSys.runSystem $ fromGraph opts vars
         runReaderT (EqSys.runVariableSystem given) vars
         return vars
   return (vars, eqs)

solveOpts ::
   (Constant a, a ~ Scalar v,
    Product v, Integrate v,
    Record rec, Node.C node) =>
   (forall s. SeqStateEqSys.Options Verify.Ignore rec s a v) ->
   StateFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   (forall s. EquationSystem Verify.Ignore rec node s a v) ->
   StateFlow.Graph node (rec (Result a)) (rec (Result v))
solveOpts opts gr sys = runST $ do
   (vars, eqs) <- setup opts gr sys
   Verify.runIgnorant $ Sys.solve eqs
   query vars

solve ::
   (Constant a, a ~ Scalar v,
    Product v, Integrate v,
    Record rec, Node.C node) =>
   StateFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   (forall s. EquationSystem Verify.Ignore rec node s a v) ->
   StateFlow.Graph node (rec (Result a)) (rec (Result v))
solve = solveOpts SeqStateEqSys.optionsDefault

solveTracked ::
   (Verify.GlobalVar (Verify.Track output) a recIdx Var.ForNodeStateScalar node,
    Constant a, a ~ Scalar v,
    Verify.GlobalVar (Verify.Track output) v recIdx Var.InStateSignal node,
    Product v, Integrate v,
    Record rec, Record.ToIndex rec ~ recIdx, Node.C node) =>
   StateFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   (forall s. EquationSystem (Verify.Track output) rec node s a v) ->
   (ME.Exceptional
      (Verify.Exception output)
      (StateFlow.Graph node (rec (Result a)) (rec (Result v))),
    Verify.Assigns output)
solveTracked gr sys = runST $ do
   (vars, eqs) <- setup SeqStateEqSys.optionsDefault gr sys
   runWriterT $ ME.runExceptionalT $ Verify.runTrack $ do
      Sys.solveBreadthFirst eqs
      MT.lift $ query vars
