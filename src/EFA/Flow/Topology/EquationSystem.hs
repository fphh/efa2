{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module EFA.Flow.Topology.EquationSystem (
   EquationSystem, Expression, RecordExpression,

   solve, solveOpts, solveTracked,

   Options, optionsDefault,
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

   ) where

import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Variable as Var
import qualified EFA.Flow.Topology as FlowTopoPlain
import EFA.Flow.Topology.Quantity (lookup)

import qualified EFA.Flow.EquationSystem as EqSys
import EFA.Flow.EquationSystem
          (constant, constantRecord, join,
           (=&=), (=%=), (=.=))

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.SystemRecord as SysRecord
import EFA.Equation.Result(Result)
import EFA.Equation.SystemRecord
          (System(System), Record, Wrap(Wrap, unwrap), Expr)
import EFA.Equation.Arithmetic
          (Sum, Product, (~*), constOne)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph as Graph

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
      (Wrap . fmap Expr.fromVariable .
       checkedLookup "variableRecord: unknown variable" lookup idx)

variable ::
   (Node.C node, FlowTopo.Lookup idx, Record rec) =>
   Record.Indexed rec (idx node) ->
   Expression mode rec node s v v
variable (Idx.Record recIdx idx) =
   fmap (Accessor.get (Record.access recIdx) . unwrap) $
   variableRecord idx



data Options mode rec s v =
   Options {
      optInOutSums ::
         SysRecord.Expr mode rec s v ->
         SysRecord.Expr mode rec s v ->
         EqSys.System mode s
   }

optionsDefault ::
   (Verify.LocalVar mode v, Record rec) =>
   Options mode rec s v
optionsDefault =
   Options { optInOutSums = (=&=) }


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
   (Verify.LocalVar mode v, Product v,
    Record rec, Node.C node) =>
   Options mode rec s v ->
   FlowTopo.DirSection node (Expr mode rec s v) ->
   EqSys.System mode s
fromTopology opts (FlowTopoPlain.Section dtime topo) =
   foldMap (fromEdge dtime) (Graph.edgeLabels topo)
   <>
   foldMap (fromSums opts) (Graph.nodeLabels topo)
   <>
   foldMap
      (\(ins,ss,outs) ->
         (flip foldMap (FlowTopo.sumIn ss) $ \s ->
            splitFactors dtime s
               FlowTopo.flowEnergyIn FlowTopo.flowXIn $ Map.elems ins)
         <>
         (flip foldMap (FlowTopo.sumOut ss) $ \s ->
            splitFactors dtime s
               FlowTopo.flowEnergyOut FlowTopo.flowXOut $ Map.elems outs))
      (Graph.graphMap topo)

fromEdge ::
   (Verify.LocalVar mode v, Product v, Record rec) =>
   Expr mode rec s v ->
   FlowTopo.Flow (Expr mode rec s v) ->
   EqSys.System mode s
fromEdge dtime
      (FlowTopo.Flow {
         FlowTopo.flowEnergyOut = eout,
         FlowTopo.flowPowerOut = pout,
         FlowTopo.flowEnergyIn = ein,
         FlowTopo.flowPowerIn = pin,
         FlowTopo.flowEta = eta
      }) =
   (eout =&= dtime ~* pout) <>
   (ein  =&= dtime ~* pin)  <>
   (pin  =&= eta ~* pout)

fromSums ::
   (Verify.LocalVar mode v, rv ~ Expr mode rec s v, Record rec) =>
   Options mode rec s v ->
   FlowTopo.Sums rv ->
   EqSys.System mode s
fromSums opts s =
   fold $ liftA2 (optInOutSums opts) (FlowTopo.sumIn s) (FlowTopo.sumOut s)

splitFactors ::
   (Verify.LocalVar mode x, Product x, Record rec,
    rx ~ Expr mode rec s x) =>
   rx ->
   rx ->
   (flow rx -> rx) ->
   (flow rx -> rx) ->
   [flow rx] ->
   EqSys.System mode s
splitFactors dtime varsum energy xfactor =
   foldMap (EqSys.splitFactors varsum energy (constOne dtime) xfactor)
   .
   NonEmpty.fetch



variables ::
   (Node.C node, Record rec, Sum v,
    Verify.GlobalVar mode v (Record.ToIndex rec) Var.Signal node) =>
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
   (Verify.GlobalVar mode v (Record.ToIndex rec) Var.Signal node,
    Product v, Record rec, Node.C node) =>
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
            FlowTopo.mapSection (Wrap . fmap Expr.fromVariable) vars
         runReaderT (EqSys.runVariableSystem given) vars
         return vars
   return (vars, eqs)

solveOpts ::
   (Product v, Record rec, Node.C node) =>
   (forall s. Options Verify.Ignore rec s v) ->
   FlowTopo.Section node (rec (Result v)) ->
   (forall s. EquationSystem Verify.Ignore rec node s v) ->
   FlowTopo.Section node (rec (Result v))
solveOpts opts gr sys = runST $ do
   (vars, eqs) <- setup opts gr sys
   Verify.runIgnorant $ Sys.solve eqs
   query vars

solve ::
   (Product v, Record rec, Node.C node) =>
   FlowTopo.Section node (rec (Result v)) ->
   (forall s. EquationSystem Verify.Ignore rec node s v) ->
   FlowTopo.Section node (rec (Result v))
solve = solveOpts optionsDefault

solveTracked ::
   (Verify.GlobalVar (Verify.Track output) v recIdx Var.Signal node,
    Product v, Record rec, Record.ToIndex rec ~ recIdx, Node.C node) =>
   FlowTopo.Section node (rec (Result v)) ->
   (forall s. EquationSystem (Verify.Track output) rec node s v) ->
   (ME.Exceptional
      (Verify.Exception output)
      (FlowTopo.Section node (rec (Result v))),
    Verify.Assigns output)
solveTracked gr sys = runST $ do
   (vars, eqs) <- setup optionsDefault gr sys
   runWriterT $ ME.runExceptionalT $ Verify.runTrack $ do
      Sys.solveBreadthFirst eqs
      MT.lift $ query vars
