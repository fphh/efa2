{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module EFA.Flow.Cumulated.EquationSystem (
   EquationSystem, Expression, RecordExpression,

   solve, solveOpts, solveTracked,

   Options, optionsDefault,
   equalInOutSums, independentInOutSums,

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

import qualified EFA.Flow.Cumulated.Quantity as CumFlow
import qualified EFA.Flow.Cumulated.Variable as Var

import qualified EFA.Flow.Topology.EquationSystem as TopoEqSys
import qualified EFA.Flow.EquationSystem as EqSys
import EFA.Flow.EquationSystem
          (constant, constantRecord, join,
           (=%=), (=.=), (=&=))

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.SystemRecord as SysRecord
import EFA.Equation.Result (Result)
import EFA.Equation.SystemRecord
          (System(System), Record, Wrap(Wrap, unwrap))
import EFA.Equation.Arithmetic
          (Sum, Product, Constant, (~+), (~*))

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

import Data.Traversable (Traversable, traverse)
import Data.Foldable (foldMap, fold)
import Data.Monoid (mempty, (<>))

import qualified Prelude as P
import Prelude hiding (lookup, init)


type
   Graph mode rec node s a =
      CumFlow.Graph node
         (SysRecord.Variable mode rec s a)

type
   Expression mode rec node s a x =
      EqSys.Expression mode (Graph mode rec node s a) s x

type
   RecordExpression mode rec node s a x =
      EqSys.RecordExpression mode (Graph mode rec node s a) rec s x

type
   EquationSystem mode rec node s a =
      EqSys.VariableSystem mode (Graph mode rec node s a) s


infix 0 =%%=, .=, %=, ?=

(=%%=) ::
   (Node.C node, CumFlow.Lookup idx, Record rec, Sys.Value mode a) =>
   idx node -> idx node ->
   EquationSystem mode rec node s a
x =%%= y  =  variableRecord x =%= variableRecord y

(.=) ::
   (Node.C node, CumFlow.Lookup idx, Record rec, Sys.Value mode a) =>
   Record.Indexed rec (idx node) -> a ->
   EquationSystem mode rec node s a
evar .= val  =  variable evar =.= constant val

(%=) ::
   (Node.C node, CumFlow.Lookup idx, Record rec, Sys.Value mode a) =>
   idx node -> rec a ->
   EquationSystem mode rec node s a
evar %= val  =  variableRecord evar =%= constantRecord val

(?=) ::
   (Node.C node, CumFlow.Lookup idx, Record rec, Sys.Value mode a) =>
   idx node -> rec (Result a) ->
   EquationSystem mode rec node s a
evar ?= val  =
   join $
   fmap
      (fold .
       liftA2
          (\rx var -> foldMap (\x -> pure var =.= constant x) rx)
          (Wrap val))
      (variableRecord evar)


variableRecord ::
   (Node.C node, CumFlow.Lookup idx, Record rec) =>
   idx node -> RecordExpression mode rec node s a a
variableRecord idx =
   EqSys.Context $
   MR.asks
      (SysRecord.exprFromVariable .
       Var.checkedLookup "variableRecord: unknown variable" CumFlow.lookup idx)

variable ::
   (Node.C node, CumFlow.Lookup idx, Record rec) =>
   Record.Indexed rec (idx node) ->
   Expression mode rec node s a a
variable (RecIdx.Record recIdx idx) =
   fmap (Accessor.get (Record.access recIdx) . unwrap) $
   variableRecord idx


data Options mode rec s a =
   Options {
      optInOutSums ::
         SysRecord.Expr mode rec s a ->
         SysRecord.Expr mode rec s a ->
         EqSys.System mode s
   }

optionsDefault ::
   (Verify.LocalVar mode a, Record rec) =>
   Options mode rec s a
optionsDefault =
   Options { optInOutSums = (=&=) }


equalInOutSums ::
   (Verify.LocalVar mode a, Record rec) =>
   Options mode rec s a ->
   Options mode rec s a
equalInOutSums opts =
   opts { optInOutSums = (=&=) }

independentInOutSums ::
   Options mode rec s a ->
   Options mode rec s a
independentInOutSums opts =
   opts { optInOutSums = const $ const mempty }


fromTopology ::
   (Verify.LocalVar mode a, Constant a, Record rec, Node.C node) =>
   Options mode rec s a ->
   CumFlow.Graph node (SysRecord.Expr mode rec s a) ->
   EqSys.System mode s
fromTopology opts topo =
   foldMap fromEdge (Graph.edgeLabels topo)
   <>
   foldMap (fromSums opts) (Graph.nodeLabels topo)
   <>
   (EqSys.withLocalVar $ \totalTime ->
      (foldMap (totalTime =&=) $
       Map.mapKeysWith (~+) (\e -> min e (Graph.reverseEdge e)) $
       fmap CumFlow.flowDTime $
       Graph.edgeLabels topo)
      <>
      foldMap
         (\(ins,ss,outs) ->
            (flip foldMap (CumFlow.sumIn ss) $ \s ->
               TopoEqSys.splitFactors totalTime s
                  CumFlow.flowEnergyIn CumFlow.flowXIn $ Map.elems ins)
            <>
            (flip foldMap (CumFlow.sumOut ss) $ \s ->
               TopoEqSys.splitFactors totalTime s
                  CumFlow.flowEnergyOut CumFlow.flowXOut $ Map.elems outs))
         (Graph.graphMap topo))

fromEdge ::
   (Sys.Value mode a, Product a, Record rec) =>
   CumFlow.Flow (SysRecord.Expr mode rec s a) ->
   EqSys.System mode s
fromEdge
      (CumFlow.Flow {
         CumFlow.flowDTime = dtime,
         CumFlow.flowEnergyOut = eout,
         CumFlow.flowPowerOut = pout,
         CumFlow.flowEnergyIn = ein,
         CumFlow.flowPowerIn = pin,
         CumFlow.flowEta = eta
      }) =
   (eout =&= dtime ~* pout) <>
   (ein  =&= dtime ~* pin)  <>
   (pin  =&= eta ~* pout)


fromSums ::
   (Verify.LocalVar mode a, Sum a, Record rec) =>
   Options mode rec s a ->
   CumFlow.Sums (SysRecord.Expr mode rec s a) ->
   EqSys.System mode s
fromSums opts s =
   let sumIn  = CumFlow.sumIn s
       sumOut = CumFlow.sumOut s
   in  fold $ liftA2 (optInOutSums opts) sumIn sumOut


withExpressionGraph ::
   (Node.C node, Record rec,
    Verify.GlobalVar mode v (Record.ToIndex rec) Var.Any node) =>
   (CumFlow.Graph node
       (RecordExpression mode rec node s v v) ->
    EquationSystem mode rec node s v) ->
   EquationSystem mode rec node s v
withExpressionGraph f =
   EqSys.VariableSystem $
      EqSys.runVariableSystem . f .
      CumFlow.mapGraph (EqSys.Context . pure . SysRecord.exprFromVariable)
         =<< MR.ask


variables ::
   (Node.C node, Record rec, Sum a,
    Verify.GlobalVar mode a (Record.ToIndex rec) Var.Any node) =>
   CumFlow.Graph node (rec (Result a)) ->
   EqSys.Writer mode s
      (CumFlow.Graph node (SysRecord.Variable mode rec s a))
variables =
   CumFlow.traverseGraph id
   .
   CumFlow.mapGraphWithVar EqSys.globalVariableFromResult

query ::
   (Node.C node, Record rec) =>
   CumFlow.Graph node (SysRecord.Variable mode rec s a) ->
   ST s (CumFlow.Graph node (rec (Result a)))
query =
   CumFlow.traverseGraph
      (traverse (fmap Result.fromMaybe . Sys.query))

setup ::
   (Verify.GlobalVar mode a (Record.ToIndex rec) Var.Any node,
    Constant a, Record rec, Node.C node) =>
   Options mode rec s a ->
   CumFlow.Graph node (rec (Result a)) ->
   EquationSystem mode rec node s a ->
   ST s
      (CumFlow.Graph node (SysRecord.Variable mode rec s a),
       Sys.T mode s ())
setup opts gr given = do
   (vars, System eqs) <-
      runWriterT $ do
         vars <- variables gr
         EqSys.runSystem $ fromTopology opts $
            CumFlow.mapGraph SysRecord.exprFromVariable vars
         runReaderT (EqSys.runVariableSystem given) vars
         return vars
   return (vars, eqs)

solveOpts ::
   (Constant a, Record rec, Node.C node) =>
   (forall s. Options Verify.Ignore rec s a) ->
   CumFlow.Graph node (rec (Result a)) ->
   (forall s. EquationSystem Verify.Ignore rec node s a) ->
   CumFlow.Graph node (rec (Result a))
solveOpts opts gr sys = runST $ do
   (vars, eqs) <- setup opts gr sys
   Verify.runIgnorant $ Sys.solve eqs
   query vars

solve ::
   (Constant a, Record rec, Node.C node) =>
   CumFlow.Graph node (rec (Result a)) ->
   (forall s. EquationSystem Verify.Ignore rec node s a) ->
   CumFlow.Graph node (rec (Result a))
solve = solveOpts optionsDefault

solveTracked ::
   (Verify.GlobalVar (Verify.Track output) a recIdx Var.Any node, Constant a,
    Record rec, Record.ToIndex rec ~ recIdx, Node.C node) =>
   CumFlow.Graph node (rec (Result a)) ->
   (forall s. EquationSystem (Verify.Track output) rec node s a) ->
   (ME.Exceptional
      (Verify.Exception output)
      (CumFlow.Graph node (rec (Result a))),
    Verify.Assigns output)
solveTracked gr sys = runST $ do
   (vars, eqs) <- setup optionsDefault gr sys
   runWriterT $ ME.runExceptionalT $ Verify.runTrack $ do
      Sys.solveBreadthFirst eqs
      MT.lift $ query vars
