{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module EFA.Flow.State.EquationSystem (
   EquationSystem, Expression, RecordExpression,

   solve, solveOpts, solveTracked,

   Options, optionsDefault, optionsBase,
   equalInOutSums, independentInOutSums,
   SeqStateEqSys.CoupleSums,
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
   withExpressionGraph,
   fromStateSystem,

   ) where

import qualified EFA.Flow.SequenceState.EquationSystem as SeqStateEqSys
import qualified EFA.Flow.SequenceState.Quantity as SeqState
import qualified EFA.Flow.SequenceState.Variable as Var
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.State.Quantity as StateFlow
import qualified EFA.Flow.Storage.EquationSystem as StorageEqSys
import qualified EFA.Flow.Storage as Storage
import qualified EFA.Flow.Topology.EquationSystem as TopoEqSys
import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Flow.EquationSystem as EqSys
import EFA.Flow.Topology.EquationSystem (fromTopology)
import EFA.Flow.EquationSystem
          (constant, constantRecord, join, (=%=), (=.=))

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.SystemRecord as SysRecord
import EFA.Equation.Arithmetic (Sum, Product, Constant, ZeroTestable)
import EFA.Equation.Result(Result)
import EFA.Equation.SystemRecord
          (System(System), Record, Wrap(Wrap, unwrap))

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Utility.Map as MapU

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
   Lookup rec node s a v idx typ =
      Lookup {
         getLookup ::
            (StateFlow.TypeOf idx ~ typ) =>
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
   Var.checkedLookup ("State.EquationSystem." ++ name)


variableRecord ::
   (Node.C node, StateFlow.Lookup idx, StateFlow.Element idx a v ~ x, Record rec) =>
   idx node -> RecordExpression mode rec node s a v x
variableRecord idx =
   EqSys.Context $
   MR.asks
      (SysRecord.exprFromVariable .
       checkedLookup "variableRecord: unknown variable" lookup idx)

variable ::
   (Node.C node, StateFlow.Lookup idx, StateFlow.Element idx a v ~ x, Record rec) =>
   Record.Indexed rec (idx node) ->
   Expression mode rec node s a v x
variable (RecIdx.Record recIdx idx) =
   fmap (Accessor.get (Record.access recIdx) . unwrap) $
   variableRecord idx


data Options mode rec s a v =
   Options {
      optTopology :: TopoEqSys.Options mode rec s v,
      optStorage :: StorageEqSys.Options mode rec s a,
      optCoupling :: SeqStateEqSys.Options mode rec s a v
   }

optionsDefault ::
   (Verify.LocalVar mode a, Sum a, Record rec) =>
   Options mode rec s a a
optionsDefault =
   optionsBase SeqStateEqSys.equalStInOutSums

optionsBase ::
   (Verify.LocalVar mode a, Sum a,
    Verify.LocalVar mode v,
    Record rec) =>
   SeqStateEqSys.CoupleSums mode rec s a v ->
   Options mode rec s a v
optionsBase couple =
   Options {
      optTopology = TopoEqSys.optionsDefault,
      optStorage = StorageEqSys.optionsDefault,
      optCoupling = SeqStateEqSys.optionsBase couple
   }


equalInOutSums ::
   (Verify.LocalVar mode v, Record rec) =>
   Options mode rec s a v ->
   Options mode rec s a v
equalInOutSums opts =
   opts { optTopology = TopoEqSys.equalInOutSums $ optTopology opts }

independentInOutSums ::
   Options mode rec s a v ->
   Options mode rec s a v
independentInOutSums opts =
   opts { optTopology = TopoEqSys.independentInOutSums $ optTopology opts }


expressionGraph ::
   (Record rec) =>
   StateFlow.Graph node
      (SysRecord.Variable mode rec s a)
      (SysRecord.Variable mode rec s v) ->
   StateFlow.Graph node
      (SysRecord.Expr mode rec s a)
      (SysRecord.Expr mode rec s v)
expressionGraph =
   StateFlow.mapGraph
      SysRecord.exprFromVariable
      SysRecord.exprFromVariable

fromGraph ::
   (Verify.LocalVar mode a, Constant a, ZeroTestable a,
    Verify.LocalVar mode v, Product v, ZeroTestable v,
    Record rec, Node.C node) =>
   Options mode rec s a v ->
   StateFlow.Graph node
      (SysRecord.Expr mode rec s a)
      (SysRecord.Expr mode rec s v) ->
   EqSys.System mode s
fromGraph opts g =
   mconcat $
      foldMap
         (fromTopology (optTopology opts) .
          FlowTopoPlain.dirSectionFromFlowGraph)
         (StateFlow.states g) :
      fromStorageSequences opts g :
      []

fromStorageSequences ::
   (Verify.LocalVar mode a, ra ~ SysRecord.Expr mode rec s a,
    Verify.LocalVar mode v, rv ~ SysRecord.Expr mode rec s v,
    Constant a, ZeroTestable a, Record rec, Node.C node) =>
   Options mode rec s a v ->
   StateFlow.Graph node
      (SysRecord.Expr mode rec s a)
      (SysRecord.Expr mode rec s v) ->
   EqSys.System mode s
fromStorageSequences opts g =
   let f sg =
          StorageEqSys.fromCarryEdges
             (optStorage opts) (Storage.edges sg)
          <>
          foldMap
             (\(s, ns) -> StorageEqSys.fromInStorages s $ Map.elems ns)
             (Storage.outEdges sg)
          <>
          foldMap
             (\(s, ns) -> StorageEqSys.fromOutStorages s $ Map.elems ns)
             (Storage.inEdges sg)
   in  (foldMap f $ StateFlow.storages g)
       <>
       (foldMap (foldMap (SeqStateEqSys.fromStorageSums (optCoupling opts))) $
        SeqState.storageSums (StateFlow.storages g) (StateFlow.states g))


withExpressionGraph ::
   (Node.C node, Record rec,
    Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForStorageStateScalar node,
    Verify.GlobalVar mode v (Record.ToIndex rec) Var.InStateSignal node) =>
   (StateFlow.Graph node
       (RecordExpression mode rec node s a v a)
       (RecordExpression mode rec node s a v v) ->
    EquationSystem mode rec node s a v) ->
   EquationSystem mode rec node s a v
withExpressionGraph f =
   EqSys.VariableSystem $
      EqSys.runVariableSystem . f .
      StateFlow.mapGraph (EqSys.Context . pure) (EqSys.Context . pure) .
      expressionGraph
         =<< MR.ask

fromStateSystem ::
   Idx.State ->
   TopoEqSys.EquationSystem mode rec node s v ->
   EquationSystem mode rec node s a v
fromStateSystem sec (EqSys.VariableSystem topoSys) =
   EqSys.VariableSystem $ do
      stateFlowGraph <- MR.ask
      MT.lift $ MR.runReaderT topoSys $
         MapU.checkedLookup
            "Sequence.EquationSystem.fromSectionSystem"
            (StateFlow.states stateFlowGraph) sec



variables ::
   (Node.C node, Record rec, Sum a, Sum v,
    Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForStorageStateScalar node,
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
   (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForStorageStateScalar node,
    Verify.GlobalVar mode v (Record.ToIndex rec) Var.InStateSignal node,
    Constant a, ZeroTestable a,
    Product v, ZeroTestable v,
    Record rec, Node.C node) =>
   Options mode rec s a v ->
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
         EqSys.runSystem $ fromGraph opts $ expressionGraph vars
         runReaderT (EqSys.runVariableSystem given) vars
         return vars
   return (vars, eqs)

solveOpts ::
   (Constant a, ZeroTestable a,
    Product v, ZeroTestable v,
    Record rec, Node.C node) =>
   (forall s. Options Verify.Ignore rec s a v) ->
   StateFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   (forall s. EquationSystem Verify.Ignore rec node s a v) ->
   StateFlow.Graph node (rec (Result a)) (rec (Result v))
solveOpts opts gr sys = runST $ do
   (vars, eqs) <- setup opts gr sys
   Verify.runIgnorant $ Sys.solve eqs
   query vars

solve ::
   (Constant a, ZeroTestable a, Record rec, Node.C node) =>
   StateFlow.Graph node (rec (Result a)) (rec (Result a)) ->
   (forall s. EquationSystem Verify.Ignore rec node s a a) ->
   StateFlow.Graph node (rec (Result a)) (rec (Result a))
solve = solveOpts optionsDefault

solveTracked ::
   (Verify.GlobalVar (Verify.Track output) a recIdx Var.ForStorageStateScalar node,
    Verify.GlobalVar (Verify.Track output) a recIdx Var.InStateSignal node,
    Constant a, ZeroTestable a,
    Record rec, Record.ToIndex rec ~ recIdx, Node.C node) =>
   StateFlow.Graph node (rec (Result a)) (rec (Result a)) ->
   (forall s. EquationSystem (Verify.Track output) rec node s a a) ->
   (ME.Exceptional
      (Verify.Exception output)
      (StateFlow.Graph node (rec (Result a)) (rec (Result a))),
    Verify.Assigns output)
solveTracked gr sys = runST $ do
   (vars, eqs) <- setup optionsDefault gr sys
   runWriterT $ ME.runExceptionalT $ Verify.runTrack $ do
      Sys.solveBreadthFirst eqs
      MT.lift $ query vars
