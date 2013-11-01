{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module EFA.Flow.Sequence.EquationSystem (
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
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as SeqIdx
import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Flow.EquationSystem as EqSys
import qualified EFA.Flow.Storage.EquationSystem as StorageEqSys
import qualified EFA.Flow.Storage as Storage
import qualified EFA.Flow.Part.Map as PartMap
import EFA.Flow.Part.Map (PartMap)
import EFA.Flow.Topology.EquationSystem (fromTopology)
import EFA.Flow.EquationSystem
          (constant, constantRecord, join,
           withLocalVar, (=&=), (=%=), (=.=))

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Flow.SequenceState.Variable as Var
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.SystemRecord as SysRecord
import EFA.Equation.Result(Result)
import EFA.Equation.SystemRecord
          (System(System), Record, Wrap(Wrap, unwrap))
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, Constant, Integrate, Scalar)

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

import Data.Map (Map)
import Data.Traversable (Traversable, traverse)
import Data.Foldable (foldMap, fold)
import Data.Monoid (Monoid, (<>), mconcat)
import Data.Tuple.HT (mapFst)

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
   Lookup rec node s a v idx typ =
      Lookup {
         getLookup ::
            (SeqFlow.TypeOf idx ~ typ) =>
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
   Var.checkedLookup ("Sequence.EquationSystem." ++ name)


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
   SeqStateEqSys.Options mode rec s a v ->
   SeqFlow.Graph node
      (SysRecord.Variable mode rec s a)
      (SysRecord.Variable mode rec s v) ->
   EqSys.System mode s
fromGraph opts gv =
   case
      SeqFlow.mapGraph
         (Wrap . fmap Expr.fromVariable)
         (Wrap . fmap Expr.fromVariable) gv of
      g ->
         mconcat $
            foldMap
               (fromTopology (SeqStateEqSys.optTopology opts) .
                FlowTopoPlain.dirSectionFromFlowGraph . snd)
               (SeqFlow.sequence g) :
            fromStorageSequences opts g :
            []

fromStorageSequences ::
   (Verify.LocalVar mode a, Constant a,
    Verify.LocalVar mode v, Record rec, Node.C node) =>
   SeqStateEqSys.Options mode rec s a v ->
   SeqFlow.Graph node
      (SysRecord.Expr mode rec s a)
      (SysRecord.Expr mode rec s v) ->
   EqSys.System mode s
fromStorageSequences opts g =
   let stoutsum sec node =
          checkedLookup "fromStorageSequences inStorages"
             SeqFlow.lookupScalar (SeqIdx.stOutSum sec node) g
       stinsum sec node =
          checkedLookup "fromStorageSequences outStorages"
             SeqFlow.lookupScalar (SeqIdx.stInSum sec node) g
       f node (Storage.Graph partMap edges, storageMap) =
          fromStorageSequence opts g node partMap storageMap
          <>
          Storage.foldInStorages
             (\sec -> fromInStorages (stoutsum sec node)) edges
          <>
          Storage.foldOutStorages
             (\sec -> fromOutStorages (stinsum sec node)) edges
   in  fold $ Map.mapWithKey f $ SeqFlow.storages g

fromStorageSequence ::
   (Verify.LocalVar mode a, ra ~ SysRecord.Expr mode rec s a,
    Verify.LocalVar mode v, rv ~ SysRecord.Expr mode rec s v,
    Sum a, Record rec, Node.C node) =>
   SeqStateEqSys.Options mode rec s a v ->
   SeqFlow.Graph node ra rv ->
   node ->
   PartMap Idx.Section ra ->
   Map Idx.Boundary ra ->
   EqSys.System mode s
fromStorageSequence opts g node partMap storageMap =
   let storages = Map.toList storageMap
   in  mconcat $
       zipWith
          (charge opts g node partMap)
          (PartMap.init partMap : map snd storages)
          (map (mapFst Idx.sectionFromBoundary) storages
           ++
           [(Nothing, PartMap.exit partMap)])

charge ::
   (Verify.LocalVar mode a, ra ~ SysRecord.Expr mode rec s a,
    Verify.LocalVar mode v, rv ~ SysRecord.Expr mode rec s v,
    Sum a, Record rec, Node.C node) =>
   SeqStateEqSys.Options mode rec s a v ->
   SeqFlow.Graph node ra rv ->
   node ->
   PartMap Idx.Section ra ->
   ra -> (Maybe Idx.Section, ra) ->
   EqSys.System mode s
charge opts g node partMap old (aug, now) =
   let carrySum sec =
          maybe (error "charge: missing section") id $
          PartMap.lookup sec partMap
       sums =
          case aug of
             Nothing -> SeqFlow.Sums Nothing Nothing
             Just sec ->
                fmap ((,) (carrySum sec)) $
                maybe (error "charge: missing sum") id $
                SeqFlow.lookupSums (Idx.secNode sec node) g
   in  SeqStateEqSys.fromStorageSums opts sums
       <>
       (condSum now (SeqFlow.sumOut sums)
        =&=
        condSum old (SeqFlow.sumIn  sums))

condSum :: (Sum a) => a -> Maybe (a,v) -> a
condSum x = maybe x (\(s,_) -> x ~+ s)


fromInStorages ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ SysRecord.Expr mode rec s x) =>
   rx -> [SeqFlow.Carry rx] ->
   EqSys.System mode s
fromInStorages stoutsum outs =
   let maxEnergies = map SeqFlow.carryMaxEnergy outs
       stEnergies  = map SeqFlow.carryEnergy outs
   in  mconcat $
       StorageEqSys.fromInStorages stoutsum outs :
       zipWith (=&=) maxEnergies
          (stoutsum : zipWith (~-) maxEnergies stEnergies)

fromOutStorages ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ SysRecord.Expr mode rec s x) =>
   rx -> [SeqFlow.Carry rx] ->
   EqSys.System mode s
fromOutStorages stinsum ins =
   (withLocalVar $ \s ->
      StorageEqSys.splitFactors s SeqFlow.carryMaxEnergy SeqFlow.carryXIn ins)
   <>
   StorageEqSys.fromOutStorages stinsum ins


variables ::
   (Node.C node, Record rec, Sum a, Sum v,
    Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForStorageSectionScalar node,
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
   (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForStorageSectionScalar node,
    Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node,
    Constant a, a ~ Scalar v,
    Product v, Integrate v,
    Record rec, Node.C node) =>
   SeqStateEqSys.Options mode rec s a v ->
   SeqFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   EquationSystem mode rec node s a v ->
   ST s
      (SeqFlow.Graph node
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
   SeqFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   (forall s. EquationSystem Verify.Ignore rec node s a v) ->
   SeqFlow.Graph node (rec (Result a)) (rec (Result v))
solveOpts opts gr sys = runST $ do
   (vars, eqs) <- setup opts gr sys
   Verify.runIgnorant $ Sys.solve eqs
   query vars

solve ::
   (Constant a, a ~ Scalar v,
    Product v, Integrate v,
    Record rec, Node.C node) =>
   SeqFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   (forall s. EquationSystem Verify.Ignore rec node s a v) ->
   SeqFlow.Graph node (rec (Result a)) (rec (Result v))
solve = solveOpts SeqStateEqSys.optionsDefault

solveTracked ::
   (Verify.GlobalVar (Verify.Track output) a recIdx Var.ForStorageSectionScalar node,
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
   (vars, eqs) <- setup SeqStateEqSys.optionsDefault gr sys
   runWriterT $ ME.runExceptionalT $ Verify.runTrack $ do
      Sys.solveBreadthFirst eqs
      MT.lift $ query vars
