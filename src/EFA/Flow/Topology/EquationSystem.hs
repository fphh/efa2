{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module EFA.Flow.Topology.EquationSystem (
   EquationSystem, Expression, RecordExpression,

   solve, solveOpts, solveTracked, solveTrackedOpts,

   Options, optionsDefault,
   cumulativeMix, realMix,
   equalInOutSums, independentInOutSums,
   fromTopology,

   splitFactors,

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

   ) where

import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Variable as Var
import qualified EFA.Flow.Topology.Index as Idx
import qualified EFA.Flow.Topology as FlowTopoPlain
import EFA.Flow.Topology.Quantity (lookup)

import qualified EFA.Flow.EquationSystem as EqSys
import EFA.Flow.EquationSystem
          (constant, constantRecord, join,
           mixSumRules, mixFactorRules, mixLevelRules,
           (=&=), (=%=), (=.=))

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.SystemRecord as SysRecord
import EFA.Equation.Result(Result)
import EFA.Equation.SystemRecord
          (System(System), Record, Wrap(Wrap, unwrap), Expr)
import EFA.Equation.Arithmetic
          (Sum, Product, (~*), ZeroTestable, constOne)

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph as Graph

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
import qualified Data.NonEmpty as NonEmpty

import Data.Traversable (Traversable, traverse)
import Data.Foldable (foldMap, fold)
import Data.Monoid (Monoid, mempty, (<>))

import qualified Prelude as P
import Prelude hiding (lookup, init)


type
   Graph mode rec node s v =
      FlowTopo.Section node
         (SysRecord.Variable mode rec s v)

type
   Expression mode rec node s v x =
      EqSys.Expression mode (Graph mode rec node s v) s x

type
   RecordExpression mode rec node s v x =
      EqSys.RecordExpression mode (Graph mode rec node s v) rec s x

type
   EquationSystem mode rec node s v =
      EqSys.VariableSystem mode (Graph mode rec node s v) s


infix 0 =%%=, .=, %=, ?=

(=%%=) ::
   (Node.C node, FlowTopo.Lookup idx,
    Record rec, Sys.Value mode v) =>
   idx node -> idx node ->
   EquationSystem mode rec node s v
x =%%= y  =  variableRecord x =%= variableRecord y

(.=) ::
   (Node.C node, FlowTopo.Lookup idx,
    Record rec, Sys.Value mode v) =>
   Record.Indexed rec (idx node) -> v ->
   EquationSystem mode rec node s v
evar .= val  =  variable evar =.= constant val

(%=) ::
   (Node.C node, FlowTopo.Lookup idx,
    Record rec, Sys.Value mode v) =>
   idx node -> rec v ->
   EquationSystem mode rec node s v
evar %= val  =  variableRecord evar =%= constantRecord val

(?=) ::
   (Node.C node, FlowTopo.Lookup idx,
    Record rec, Sys.Value mode v) =>
   idx node -> rec (Result v) ->
   EquationSystem mode rec node s v
evar ?= val  =
   join $
   fmap
      (fold .
       liftA2
          (\rx var -> foldMap (\x -> pure var =.= constant x) rx)
          (Wrap val))
      (variableRecord evar)


checkedLookup ::
   (Node.C node, Var.FormatIndex idx) =>
   String -> (idx node -> t -> Maybe b) -> idx node -> t -> b
checkedLookup name =
   Var.checkedLookup $ "Topology.EquationSystem." ++ name


variableRecord ::
   (Node.C node, FlowTopo.Lookup idx, Record rec) =>
   idx node -> RecordExpression mode rec node s v v
variableRecord idx =
   EqSys.Context $
   MR.asks
      (SysRecord.exprFromVariable .
       checkedLookup "variableRecord: unknown variable" lookup idx)

variable ::
   (Node.C node, FlowTopo.Lookup idx, Record rec) =>
   Record.Indexed rec (idx node) ->
   Expression mode rec node s v v
variable (RecIdx.Record recIdx idx) =
   fmap (Accessor.get (Record.access recIdx) . unwrap) $
   variableRecord idx



data Options mode rec s v =
   Options {
      optInOutSums ::
         Expr mode rec s v ->
         Expr mode rec s v ->
         EqSys.System mode s,
      optEqualFactorsOut, optEqualFactorsIn, optEqualEta ::
         Expr mode rec s v ->
         EqSys.System mode s
   }

optionsDefault ::
   (Verify.LocalVar mode v, Record rec) =>
   Options mode rec s v
optionsDefault =
   Options {
      optInOutSums = (=&=),
      optEqualFactorsOut = const mempty,
      optEqualFactorsIn = const mempty,
      optEqualEta = const mempty
   }

cumulativeMix ::
   (Verify.LocalVar mode v, Record rec) =>
   Options mode rec s v ->
   Options mode rec s v
cumulativeMix opts =
   opts {
      optEqualFactorsOut = const mempty,
      optEqualFactorsIn = const mempty,
      optEqualEta = const mempty
   }

realMix ::
   (Verify.LocalVar mode v, Sum v, Record rec) =>
   Options mode rec s v ->
   Options mode rec s v
realMix opts =
   opts {
      optEqualFactorsOut = mixLevelRules Idx.Out,
      optEqualFactorsIn  = mixLevelRules Idx.In,
      optEqualEta = mixFactorRules
   }


equalInOutSums ::
   (Verify.LocalVar mode v, Record rec) =>
   Options mode rec s v ->
   Options mode rec s v
equalInOutSums opts =
   opts { optInOutSums = (=&=) }

independentInOutSums ::
   Options mode rec s v ->
   Options mode rec s v
independentInOutSums opts =
   opts { optInOutSums = const $ const mempty }


fromTopology ::
   (Verify.LocalVar mode v, Product v, ZeroTestable v,
    Record rec, Node.C node) =>
   Options mode rec s v ->
   FlowTopo.DirSection node (Expr mode rec s v) ->
   EqSys.System mode s
fromTopology opts gr@(FlowTopoPlain.Section dtime topo) =
   mixFactorRules dtime
   <>
   foldMap (fromEdge opts dtime) (Graph.edgeLabels topo)
   <>
   foldMap (fromSums opts) (Graph.nodeLabels topo)
   <>
   (EqSys.withLocalVar $ \one ->
      FlowTopo.foldMapDir (\signal -> one =&= constOne signal) gr
      <>
      foldMap
         (\(ins,ss,outs) ->
            (flip foldMap (FlowTopo.sumIn ss) $ \s ->
               splitFactors one s
                  FlowTopo.flowEnergyIn FlowTopo.flowXIn $ Map.elems ins)
            <>
            (flip foldMap (FlowTopo.sumOut ss) $ \s ->
               splitFactors one s
                  FlowTopo.flowEnergyOut FlowTopo.flowXOut $ Map.elems outs))
         (Graph.graphMap topo))

fromEdge ::
   (Verify.LocalVar mode v, Product v, ZeroTestable v, Record rec) =>
   Options mode rec s v ->
   Expr mode rec s v ->
   FlowTopo.Flow (Expr mode rec s v) ->
   EqSys.System mode s
fromEdge opts dtime
      (FlowTopo.Flow {
         FlowTopo.flowXOut = xout,
         FlowTopo.flowEnergyOut = eout,
         FlowTopo.flowPowerOut = pout,
         FlowTopo.flowXIn = xin,
         FlowTopo.flowEnergyIn = ein,
         FlowTopo.flowPowerIn = pin,
         FlowTopo.flowEta = eta
      }) =
   (eout =&= dtime ~* pout) <>
   (ein  =&= dtime ~* pin)  <>
   (pin  =&= eta ~* pout) <>
   mixSumRules pout <>
   mixSumRules pin <>
   mixSumRules eout <>
   mixSumRules ein <>
   optEqualFactorsOut opts xout <>
   optEqualFactorsIn opts xin <>
   optEqualEta opts eta

fromSums ::
   (Verify.LocalVar mode v, rv ~ Expr mode rec s v, Record rec) =>
   Options mode rec s v ->
   FlowTopo.Sums rv ->
   EqSys.System mode s
fromSums opts s =
   fold $ liftA2 (optInOutSums opts) (FlowTopo.sumIn s) (FlowTopo.sumOut s)

splitFactors ::
   (Verify.LocalVar mode x, Product x, ZeroTestable x, Record rec,
    rx ~ Expr mode rec s x) =>
   rx ->
   rx ->
   (flow rx -> rx) ->
   (flow rx -> rx) ->
   [flow rx] ->
   EqSys.System mode s
splitFactors one varsum energy xfactor =
   foldMap (EqSys.splitFactors varsum energy one xfactor)
   .
   NonEmpty.fetch



withExpressionGraph ::
   (Node.C node, Record rec,
    Verify.GlobalVar mode v (Var.RecordSignal rec node)) =>
   (FlowTopo.Section node
       (RecordExpression mode rec node s v v) ->
    EquationSystem mode rec node s v) ->
   EquationSystem mode rec node s v
withExpressionGraph f =
   EqSys.VariableSystem $
      EqSys.runVariableSystem . f .
      FlowTopo.mapSection (EqSys.Context . pure . SysRecord.exprFromVariable)
         =<< MR.ask


variables ::
   (Node.C node, Record rec, Sum v,
    Verify.GlobalVar mode v (Var.RecordSignal rec node)) =>
   FlowTopo.Section node (rec (Result v)) ->
   EqSys.Writer mode s
      (FlowTopo.Section node
         (SysRecord.Variable mode rec s v))
variables =
   FlowTopo.traverseSection id
   .
   FlowTopo.mapSectionWithVar EqSys.globalVariableFromResult

query ::
   (Node.C node, Record rec) =>
   FlowTopo.Section node (SysRecord.Variable mode rec s v) ->
   ST s (FlowTopo.Section node (rec (Result v)))
query =
   FlowTopo.traverseSection
      (traverse (fmap Result.fromMaybe . Sys.query))


setup ::
   (Verify.GlobalVar mode v (Var.RecordSignal rec node),
    Product v, ZeroTestable v, Record rec, Node.C node) =>
   Options mode rec s v ->
   FlowTopo.Section node (rec (Result v)) ->
   EquationSystem mode rec node s v ->
   ST s
      (FlowTopo.Section node
         (SysRecord.Variable mode rec s v),
       Sys.T mode s ())
setup opts gr given = do
   (vars, System eqs) <-
      runWriterT $ do
         vars <- variables gr
         EqSys.runSystem $ fromTopology opts $
            FlowTopoPlain.dirSectionFromFlowGraph $
            FlowTopo.mapSection SysRecord.exprFromVariable vars
         runReaderT (EqSys.runVariableSystem given) vars
         return vars
   return (vars, eqs)

solveOpts ::
   (Product v, ZeroTestable v, Record rec, Node.C node) =>
   (forall s. Options Verify.Ignore rec s v) ->
   FlowTopo.Section node (rec (Result v)) ->
   (forall s. EquationSystem Verify.Ignore rec node s v) ->
   FlowTopo.Section node (rec (Result v))
solveOpts opts gr sys = runST $ do
   (vars, eqs) <- setup opts gr sys
   Verify.runIgnorant $ Sys.solve eqs
   query vars

solve ::
   (Product v, ZeroTestable v, Record rec, Node.C node) =>
   FlowTopo.Section node (rec (Result v)) ->
   (forall s. EquationSystem Verify.Ignore rec node s v) ->
   FlowTopo.Section node (rec (Result v))
solve = solveOpts optionsDefault

solveTrackedOpts ::
   (Verify.GlobalVar (Verify.Track output) v (Var.RecordSignal rec node),
    Product v, ZeroTestable v, Record rec,
    Node.C node) =>
   (forall s. Options (Verify.Track output) rec s v) ->
   FlowTopo.Section node (rec (Result v)) ->
   (forall s. EquationSystem (Verify.Track output) rec node s v) ->
   (ME.Exceptional
      (Verify.Exception output)
      (FlowTopo.Section node (rec (Result v))),
    Verify.Assigns output)
solveTrackedOpts opts gr sys = runST $ do
   (vars, eqs) <- setup opts gr sys
   runWriterT $ ME.runExceptionalT $ Verify.runTrack $ do
      Sys.solveBreadthFirst eqs
      MT.lift $ query vars

solveTracked ::
   (Verify.GlobalVar (Verify.Track output) v (Var.RecordSignal rec node),
    Product v, ZeroTestable v, Record rec,
    Node.C node) =>
   FlowTopo.Section node (rec (Result v)) ->
   (forall s. EquationSystem (Verify.Track output) rec node s v) ->
   (ME.Exceptional
      (Verify.Exception output)
      (FlowTopo.Section node (rec (Result v))),
    Verify.Assigns output)
solveTracked = solveTrackedOpts optionsDefault
