{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module EFA.Flow.Sequence.EquationSystem (
   EquationSystem, Expression, RecordExpression,

   solve, solveFromMeasurement, solveTracked,

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

import qualified EFA.Flow.Sequence.Quantity as SeqFlow

import qualified EFA.Flow.Quantity as Quant
import qualified EFA.Flow.EquationSystem as EqSys
import EFA.Flow.EquationSystem
          (constant, constantRecord, join, fromTopology,
           splitScalarEqs, withLocalVar, (=&=), (=%=), (=.=))

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.SystemRecord as SysRecord
import EFA.Equation.Result(Result(..))
import EFA.Equation.SystemRecord
          (System(System), Record, Wrap(Wrap, unwrap))
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, Constant, Integrate, Scalar)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Utility.Map as MapU

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

import Data.Map (Map)
import Data.Traversable (Traversable, traverse)
import Data.Foldable (foldMap, fold)
import Data.Monoid (Monoid, (<>), mconcat)
import Data.Tuple.HT (mapFst, mapSnd)

import qualified Prelude as P
import Prelude hiding (lookup, init)


type
   Graph mode rec node s a v =
      SeqFlow.Graph node
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
   (Node.C node, SeqFlow.Lookup idx, SeqFlow.Element idx a v ~ x,
    Record rec, Sys.Value mode x) =>
   idx node -> idx node ->
   EquationSystem mode rec node s a v
x =%%= y  =  variableRecord x =%= variableRecord y

(.=) ::
   (Node.C node, SeqFlow.Lookup idx, SeqFlow.Element idx a v ~ x,
    Record rec, Sys.Value mode x) =>
   Record.Indexed rec (idx node) -> x ->
   EquationSystem mode rec node s a v
evar .= val  =  variable evar =.= constant val

(%=) ::
   (Node.C node, SeqFlow.Lookup idx, SeqFlow.Element idx a v ~ x,
    Record rec, Sys.Value mode x) =>
   idx node -> rec x ->
   EquationSystem mode rec node s a v
evar %= val  =  variableRecord evar =%= constantRecord val

(?=) ::
   (Node.C node, SeqFlow.Lookup idx, SeqFlow.Element idx a v ~ x,
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
            (SeqFlow.Environment idx ~ env) =>
            idx node ->
            SeqFlow.Graph node
               (SysRecord.Variable mode rec s a)
               (SysRecord.Variable mode rec s v) ->
            Maybe
               (SysRecord.Variable mode rec s (SeqFlow.Element idx a v))
      }

lookup ::
   (SeqFlow.Lookup idx, Node.C node) =>
   idx node ->
   SeqFlow.Graph node
      (SysRecord.Variable mode rec s a)
      (SysRecord.Variable mode rec s v) ->
   Maybe
      (SysRecord.Variable mode rec s (SeqFlow.Element idx a v))
lookup =
   getLookup $
   SeqFlow.switchPart
      (Lookup $ SeqFlow.lookup)
      (Lookup $ SeqFlow.lookup)

checkedLookup ::
   (Node.C node, Var.FormatIndex idx) =>
   String -> (idx node -> t -> Maybe b) -> idx node -> t -> b
checkedLookup name =
   Var.checkedLookup ("EquationSystem." ++ name)


variableRecord ::
   (Node.C node, SeqFlow.Lookup idx, SeqFlow.Element idx a v ~ x, Record rec) =>
   idx node -> RecordExpression mode rec node s a v x
variableRecord idx =
   EqSys.Context $
   MR.asks
      (Wrap . fmap Expr.fromVariable .
       checkedLookup "variableRecord: unknown variable" lookup idx)

variable ::
   (Node.C node, SeqFlow.Lookup idx, SeqFlow.Element idx a v ~ x, Record rec) =>
   Record.Indexed rec (idx node) ->
   Expression mode rec node s a v x
variable (Idx.Record recIdx idx) =
   fmap (Accessor.get (Record.access recIdx) . unwrap) $
   variableRecord idx


fromGraph ::
   (Verify.LocalVar mode a, Constant a, a ~ Scalar v,
    Verify.LocalVar mode v, Product v, Integrate v,
    Record rec, Node.C node) =>
   Bool ->
   SeqFlow.Graph node
      (SysRecord.Variable mode rec s a)
      (SysRecord.Variable mode rec s v) ->
   EqSys.System mode s
fromGraph equalInOutSums gv =
   case
      SeqFlow.mapGraph
         (Wrap . fmap Expr.fromVariable)
         (Wrap . fmap Expr.fromVariable) gv of
      g ->
         mconcat $
            foldMap
               (uncurry (fromTopology equalInOutSums) .
                mapSnd Quant.dirFromFlowGraph . snd)
               (SeqFlow.sequence g) :
            fromStorageSequences g :
            []

fromStorageSequences ::
   (Verify.LocalVar mode a, Constant a, Record rec, Node.C node) =>
   SeqFlow.Graph node
      (SysRecord.Expr mode rec s a)
      (SysRecord.Expr mode rec s v) ->
   EqSys.System mode s
fromStorageSequences g =
   let stoutsum sec node =
          checkedLookup "fromStorageSequences inStorages"
             SeqFlow.lookupStOutSum (Idx.ForNode (Idx.StOutSum sec) node) g
       stinsum sec node =
          checkedLookup "fromStorageSequences outStorages"
             SeqFlow.lookupStInSum (Idx.ForNode (Idx.StInSum sec) node) g
       f node (initExit, storageMap, edges) =
          fromStorageSequence g node initExit storageMap
          <>
          (fold $
           Map.mapWithKey
              (\sec outs ->
                 fromInStorages (stoutsum sec node) (Map.elems outs)) $
           MapU.curry "EquationSystem.fromStorageSequences.fromInStorages"
              (\(Idx.StorageEdge from to) -> (from, to))
              edges)
          <>
          (fold $
           Map.mapWithKey
              (\sec ins ->
                 fromOutStorages (stinsum sec node) (Map.elems ins)) $
           MapU.curry "EquationSystem.fromStorageSequences.fromOutStorages"
              (\(Idx.StorageEdge from to) -> (to, from))
              edges)
   in  fold $ Map.mapWithKey f $ SeqFlow.storages g

fromStorageSequence ::
   (Sys.Value mode a, Sum a, Record rec, Node.C node,
    ra ~ SysRecord.Expr mode rec s a,
    rv ~ SysRecord.Expr mode rec s v) =>
   SeqFlow.Graph node ra rv ->
   node ->
   (ra, ra) ->
   Map Idx.Boundary ra ->
   EqSys.System mode s
fromStorageSequence g node (init,exit) storageMap =
   let storages = Map.toList storageMap
   in  mconcat $
       zipWith
          (charge g node)
          (init : map snd storages)
          (map (mapFst Idx.sectionFromBoundary) storages
           ++
           [(Nothing, exit)])

charge ::
   (Sys.Value mode a, Sum a, Record rec, Node.C node,
    ra ~ SysRecord.Expr mode rec s a) =>
   SeqFlow.Graph node ra rv ->
   node ->
   ra -> (Maybe Idx.Section, ra) ->
   EqSys.System mode s
charge g node old (aug, now) =
   let sums =
          case aug of
             Nothing -> SeqFlow.Sums Nothing Nothing
             Just sec ->
                maybe (error "missing sum") id $
                SeqFlow.lookupSums (Idx.secNode sec node) g
   in  condSum now (SeqFlow.sumOut sums)
       =&=
       condSum old (SeqFlow.sumIn  sums)

condSum :: (Sum a) => a -> Maybe (SeqFlow.Sum a v) -> a
condSum x = maybe x (\s -> x ~+ SeqFlow.carrySum s)


fromInStorages ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ SysRecord.Expr mode rec s x) =>
   rx -> [SeqFlow.Carry rx] ->
   EqSys.System mode s
fromInStorages stoutsum outs =
   let maxEnergies = map SeqFlow.carryMaxEnergy outs
       stEnergies  = map SeqFlow.carryEnergy outs
   in  mconcat $
       EqSys.fromInStorages stoutsum outs :
       zipWith (=&=) maxEnergies
          (stoutsum : zipWith (~-) maxEnergies stEnergies)

fromOutStorages ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ SysRecord.Expr mode rec s x) =>
   rx -> [SeqFlow.Carry rx] ->
   EqSys.System mode s
fromOutStorages stinsum ins =
   (withLocalVar $ \s ->
      splitScalarEqs s SeqFlow.carryMaxEnergy SeqFlow.carryXIn ins)
   <>
   EqSys.fromOutStorages stinsum ins


variables ::
   (Node.C node, Record rec, Sum a, Sum v,
    Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeSectionScalar node,
    Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node) =>
   SeqFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   EqSys.Writer mode s
      (SeqFlow.Graph node
         (SysRecord.Variable mode rec s a)
         (SysRecord.Variable mode rec s v))
variables =
   SeqFlow.traverseGraph id id
   .
   SeqFlow.mapGraphWithVar
      EqSys.globalVariableFromResult
      EqSys.globalVariableFromResult

query ::
   (Node.C node, Record rec) =>
   SeqFlow.Graph node
      (SysRecord.Variable mode rec s a)
      (SysRecord.Variable mode rec s v) ->
   ST s
      (SeqFlow.Graph node
         (rec (Result a))
         (rec (Result v)))
query =
   SeqFlow.traverseGraph
      (traverse (fmap Result.fromMaybe . Sys.query))
      (traverse (fmap Result.fromMaybe . Sys.query))


setup ::
   (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeSectionScalar node,
    Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node,
    Constant a, a ~ Scalar v,
    Product v, Integrate v,
    Record rec, Node.C node) =>
   Bool ->
   SeqFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   EquationSystem mode rec node s a v ->
   ST s
      (SeqFlow.Graph node
         (SysRecord.Variable mode rec s a)
         (SysRecord.Variable mode rec s v),
       Sys.T mode s ())
setup equalInOutSums gr given = do
   (vars, System eqs) <-
      runWriterT $ do
         vars <- variables gr
         EqSys.runSystem $ fromGraph equalInOutSums vars
         runReaderT (EqSys.runVariableSystem given) vars
         return vars
   return (vars, eqs)

solveGen ::
   (Constant a, a ~ Scalar v,
    Product v, Integrate v,
    Record rec, Node.C node) =>
   Bool ->
   SeqFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   (forall s. EquationSystem Verify.Ignore rec node s a v) ->
   SeqFlow.Graph node (rec (Result a)) (rec (Result v))
solveGen equalInOutSums gr sys = runST $ do
   (vars, eqs) <- setup equalInOutSums gr sys
   Verify.runIgnorant $ Sys.solve eqs
   query vars

solve ::
   (Constant a, a ~ Scalar v,
    Product v, Integrate v,
    Record rec, Node.C node) =>
   SeqFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   (forall s. EquationSystem Verify.Ignore rec node s a v) ->
   SeqFlow.Graph node (rec (Result a)) (rec (Result v))
solve = solveGen True

solveFromMeasurement ::
   (Constant a, a ~ Scalar v,
    Product v, Integrate v,
    Record rec, Node.C node) =>
   SeqFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   (forall s. EquationSystem Verify.Ignore rec node s a v) ->
   SeqFlow.Graph node (rec (Result a)) (rec (Result v))
solveFromMeasurement = solveGen False

solveTracked ::
   (Verify.GlobalVar (Verify.Track output) a recIdx Var.ForNodeSectionScalar node,
    Constant a, a ~ Scalar v,
    Verify.GlobalVar (Verify.Track output) v recIdx Var.InSectionSignal node,
    Product v, Integrate v,
    Record rec, Record.ToIndex rec ~ recIdx, Node.C node) =>
   SeqFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   (forall s. EquationSystem (Verify.Track output) rec node s a v) ->
   (ME.Exceptional
      (Verify.Exception output)
      (SeqFlow.Graph node (rec (Result a)) (rec (Result v))),
    Verify.Assigns output)
solveTracked gr sys = runST $ do
   (vars, eqs) <- setup True gr sys
   runWriterT $ ME.runExceptionalT $ Verify.runTrack $ do
      Sys.solveBreadthFirst eqs
      MT.lift $ query vars
