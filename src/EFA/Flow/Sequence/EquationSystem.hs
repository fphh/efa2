{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module EFA.Flow.Sequence.EquationSystem (
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
   fromSectionSystem,

   ) where

import qualified EFA.Flow.SequenceState.EquationSystem as SeqStateEqSys
import qualified EFA.Flow.SequenceState.Quantity as SeqState
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Sequence.Variable as Var
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Topology.EquationSystem as TopoEqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Flow.EquationSystem as EqSys
import qualified EFA.Flow.Storage.EquationSystem as StorageEqSys
import qualified EFA.Flow.Storage as Storage
import qualified EFA.Flow.Part.Map as PartMap
import EFA.Flow.Topology.EquationSystem (fromTopology)
import EFA.Flow.EquationSystem
          (constant, constantRecord, join,
           withLocalVar, (=&=), (=%=), (=.=))

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.SystemRecord as SysRecord
import EFA.Equation.Result(Result)
import EFA.Equation.SystemRecord
          (System(System), Record, Wrap(Wrap, unwrap))
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, Constant, ZeroTestable, Integrate, Scalar)

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

import Data.Map (Map)
import Data.Traversable (Traversable, traverse)
import Data.Foldable (foldMap, fold)
import Data.Monoid (Monoid, (<>), mconcat)

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
      (SysRecord.exprFromVariable .
       checkedLookup "variableRecord: unknown variable" lookup idx)

variable ::
   (Node.C node, SeqFlow.Lookup idx, SeqFlow.Element idx a v ~ x, Record rec) =>
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
   (Verify.LocalVar mode a, Constant a, ZeroTestable a, a ~ Scalar v,
    Verify.LocalVar mode v, Sum v, Integrate v,
    Record rec) =>
   Options mode rec s a v
optionsDefault =
   optionsBase SeqStateEqSys.integrateStInOutSums StorageEqSys.classOne

optionsBase ::
   (Verify.LocalVar mode a, Sum a,
    Verify.LocalVar mode v, Sum v,
    Record rec) =>
   SeqStateEqSys.CoupleSums mode rec s a v ->
   StorageEqSys.One mode rec s a ->
   Options mode rec s a v
optionsBase couple one =
   Options {
      optTopology = TopoEqSys.realMix $ TopoEqSys.optionsDefault,
      optStorage = StorageEqSys.realMix $ StorageEqSys.optionsBase one,
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
   SeqFlow.Graph node
      (SysRecord.Variable mode rec s a)
      (SysRecord.Variable mode rec s v) ->
   SeqFlow.Graph node
      (SysRecord.Expr mode rec s a)
      (SysRecord.Expr mode rec s v)
expressionGraph =
   SeqFlow.mapGraph
      SysRecord.exprFromVariable
      SysRecord.exprFromVariable

fromGraph ::
   (Verify.LocalVar mode a, Product a, ZeroTestable a,
    Verify.LocalVar mode v, Product v, ZeroTestable v,
    Record rec, Node.C node) =>
   Options mode rec s a v ->
   SeqFlow.Graph node
      (SysRecord.Expr mode rec s a)
      (SysRecord.Expr mode rec s v) ->
   EqSys.System mode s
fromGraph opts g =
   mconcat $
      foldMap
         (fromTopology (optTopology opts) .
          FlowTopoPlain.dirSectionFromFlowGraph . snd)
         (SeqFlow.sequence g) :
      fromStorageSequences opts g :
      []

fromStorageSequences ::
   (Verify.LocalVar mode a, Product a, ZeroTestable a,
    Verify.LocalVar mode v, Record rec, Node.C node) =>
   Options mode rec s a v ->
   SeqFlow.Graph node
      (SysRecord.Expr mode rec s a)
      (SysRecord.Expr mode rec s v) ->
   EqSys.System mode s
fromStorageSequences opts g =
   let f sumMap (sg@(Storage.Graph partMap edges), storageMap) =
          StorageEqSys.fromCarryEdges (optStorage opts) edges
          <>
          fromStorageSequence (optCoupling opts) sumMap
             (PartMap.init partMap, PartMap.exit partMap) storageMap
          <>
          foldMap
             (\(s, ns) -> fromInStorages (optStorage opts) s $ Map.elems ns)
             (Storage.outEdges sg)
          <>
          foldMap
             (\(s, ns) -> fromOutStorages (optStorage opts) s $ Map.elems ns)
             (Storage.inEdges sg)
   in  fold $
       Map.intersectionWith f
          (SeqState.storageSums
              (fmap fst $ SeqFlow.storages g) (fmap snd $ SeqFlow.sequence g))
          (SeqFlow.storages g)

fromStorageSequence ::
   (Verify.LocalVar mode a, ra ~ SysRecord.Expr mode rec s a,
    Verify.LocalVar mode v, rv ~ SysRecord.Expr mode rec s v,
    Sum a, Record rec) =>
   SeqStateEqSys.Options mode rec s a v ->
   Map Idx.Section (FlowTopo.Sums (ra,rv)) ->
   (ra, ra) ->
   Map Idx.Boundary ra ->
   EqSys.System mode s
fromStorageSequence opts sumMap (init, exit) storageMap =
   let noSum = SeqFlow.Sums Nothing Nothing
       storages =
          Map.elems $
          MapU.checkedZipWith
             "Sequence.EquationSystem.fromStorageSequence"
             (,)
             (Map.insert Idx.initial noSum $
              Map.mapKeys Idx.afterSection sumMap)
             storageMap
   in  mconcat $
       zipWith
          (charge opts)
          (init : map snd storages)
          (storages ++ [(noSum, exit)])

charge ::
   (Verify.LocalVar mode a, ra ~ SysRecord.Expr mode rec s a,
    Verify.LocalVar mode v, rv ~ SysRecord.Expr mode rec s v,
    Sum a, Record rec) =>
   SeqStateEqSys.Options mode rec s a v ->
   ra -> (FlowTopo.Sums (ra,rv), ra) ->
   EqSys.System mode s
charge opts old (sums, now) =
   SeqStateEqSys.fromStorageSums opts sums
   <>
   (condSum now (SeqFlow.sumOut sums)
    =&=
    condSum old (SeqFlow.sumIn  sums))

condSum :: (Sum a) => a -> Maybe (a,v) -> a
condSum x = maybe x (\(s,_) -> x ~+ s)


fromInStorages ::
   (Verify.LocalVar mode x, Product x, ZeroTestable x, Record rec,
    rx ~ SysRecord.Expr mode rec s x) =>
   StorageEqSys.Options mode rec s x ->
   rx -> [SeqFlow.Carry rx] ->
   EqSys.System mode s
fromInStorages opts stoutsum outs =
   let maxEnergies = map SeqFlow.carryMaxEnergy outs
       stEnergies  = map SeqFlow.carryEnergy outs
   in  mconcat $
       StorageEqSys.fromInStorages opts stoutsum outs :
       zipWith (=&=) maxEnergies
          (stoutsum : zipWith (~-) maxEnergies stEnergies)

fromOutStorages ::
   (Verify.LocalVar mode x, Product x, ZeroTestable x, Record rec,
    rx ~ SysRecord.Expr mode rec s x) =>
   StorageEqSys.Options mode rec s x ->
   rx -> [SeqFlow.Carry rx] ->
   EqSys.System mode s
fromOutStorages opts stinsum ins =
   (withLocalVar $ \s ->
      StorageEqSys.splitFactors opts s SeqFlow.carryMaxEnergy SeqFlow.carryXIn ins)
   <>
   StorageEqSys.fromOutStorages opts stinsum ins


withExpressionGraph ::
   (Node.C node, Record rec,
    Verify.GlobalVar mode a (Var.RecordScalar rec node),
    Verify.GlobalVar mode v (Var.RecordSignal rec node)) =>
   (SeqFlow.Graph node
       (RecordExpression mode rec node s a v a)
       (RecordExpression mode rec node s a v v) ->
    EquationSystem mode rec node s a v) ->
   EquationSystem mode rec node s a v
withExpressionGraph f =
   EqSys.VariableSystem $
      EqSys.runVariableSystem . f .
      SeqFlow.mapGraph (EqSys.Context . pure) (EqSys.Context . pure) .
      expressionGraph
         =<< MR.ask

fromSectionSystem ::
   Idx.Section ->
   TopoEqSys.EquationSystem mode rec node s v ->
   EquationSystem mode rec node s a v
fromSectionSystem sec (EqSys.VariableSystem topoSys) =
   EqSys.VariableSystem $ do
      seqFlowGraph <- MR.ask
      MT.lift $ MR.runReaderT topoSys $ snd $
         MapU.checkedLookup
            "Sequence.EquationSystem.fromSectionSystem"
            (SeqFlow.sequence seqFlowGraph) sec


variables ::
   (Node.C node, Record rec, Sum a, Sum v,
    Verify.GlobalVar mode a (Var.RecordScalar rec node),
    Verify.GlobalVar mode v (Var.RecordSignal rec node)) =>
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
   (Verify.GlobalVar mode a (Var.RecordScalar rec node),
    Verify.GlobalVar mode v (Var.RecordSignal rec node),
    Product a, ZeroTestable a,
    Product v, ZeroTestable v,
    Record rec, Node.C node) =>
   Options mode rec s a v ->
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
         EqSys.runSystem $ fromGraph opts $ expressionGraph vars
         runReaderT (EqSys.runVariableSystem given) vars
         return vars
   return (vars, eqs)

solveOpts ::
   (Product a, ZeroTestable a,
    Product v, ZeroTestable v,
    Record rec, Node.C node) =>
   (forall s. Options Verify.Ignore rec s a v) ->
   SeqFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   (forall s. EquationSystem Verify.Ignore rec node s a v) ->
   SeqFlow.Graph node (rec (Result a)) (rec (Result v))
solveOpts opts gr sys = runST $ do
   (vars, eqs) <- setup opts gr sys
   Verify.runIgnorant $ Sys.solve eqs
   query vars

solve ::
   (Constant a, ZeroTestable a, a ~ Scalar v,
    Product v, ZeroTestable v, Integrate v,
    Record rec, Node.C node) =>
   SeqFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   (forall s. EquationSystem Verify.Ignore rec node s a v) ->
   SeqFlow.Graph node (rec (Result a)) (rec (Result v))
solve = solveOpts optionsDefault

solveTracked ::
   (Verify.GlobalVar (Verify.Track output) a (Var.RecordScalar rec node),
    Constant a, ZeroTestable a, a ~ Scalar v,
    Verify.GlobalVar (Verify.Track output) v (Var.RecordSignal rec node),
    Product v, ZeroTestable v, Integrate v,
    Record rec, Node.C node) =>
   SeqFlow.Graph node (rec (Result a)) (rec (Result v)) ->
   (forall s. EquationSystem (Verify.Track output) rec node s a v) ->
   (ME.Exceptional
      (Verify.Exception output)
      (SeqFlow.Graph node (rec (Result a)) (rec (Result v))),
    Verify.Assigns output)
solveTracked gr sys = runST $ do
   (vars, eqs) <- setup optionsDefault gr sys
   runWriterT $ ME.runExceptionalT $ Verify.runTrack $ do
      Sys.solveBreadthFirst eqs
      MT.lift $ query vars
