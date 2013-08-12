{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EFA.Graph.SequenceFlow.EquationSystem (
   EquationSystem, Expression, RecordExpression,

   solve, solveFromMeasurement, solveTracked,

   constant,
   constantRecord,
   liftF, liftF2,
   sqrt,

   Record, Wrap(Wrap, unwrap),

   (=.=), (.=),
   (=%=), (%=), (=%%=),
   (?=), Result(..),
   variable,
   variableRecord,

   ) where

import qualified EFA.Graph.SequenceFlow.Quantity as SeqFlow
import qualified EFA.Graph.SequenceFlow as SeqFlowPlain

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.SystemRecord as SysRecord
import EFA.Equation.SystemRecord
          (System(System), Record, Wrap(Wrap, unwrap))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph as Gr

import EFA.Report.FormatValue (FormatValue)

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Result as Result
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, (~*), (~/),
           Constant, zero,
           Integrate, Scalar, integrate)
import EFA.Equation.Result(Result(..))

import qualified EFA.Utility.Map as MapU
import EFA.Utility ((>>!))

import UniqueLogic.ST.TF.Expression ((=:=))
import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.System as Sys

import qualified Data.Accessor.Basic as Accessor

import qualified Control.Monad.Exception.Synchronous as ME
import qualified Control.Monad.Trans.Reader as MR
import qualified Control.Monad.Trans.Class as MT
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)

import Control.Monad.ST (ST, runST)
import Control.Monad (liftM2, guard)

import Control.Applicative (Applicative, pure, liftA, liftA2)
import Control.Category ((.))

import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty

import Data.Map (Map)
import Data.Traversable (Traversable, traverse, for, sequenceA)
import Data.Foldable (foldMap, fold)
import Data.Monoid (Monoid, (<>), mempty, mappend, mconcat)
import Data.Tuple.HT (mapFst)

import qualified Prelude as P
import Prelude hiding (lookup, init, sqrt, (.))


type
   BK mode rec node s a v =
      ReaderT
         (SeqFlow.Graph node
            (SysRecord.Variable mode rec s a)
            (SysRecord.Variable mode rec s v))
         (WriterT (System mode s) (ST s))


type Expr mode = Expr.T mode

type
   Expression mode rec node s a v x =
      Bookkeeping mode rec node s a v (Expr mode s x)

type
   RecordExpression mode rec node s a v x =
      Bookkeeping mode rec node s a v (Wrap rec (Expr mode s x))


newtype
   Bookkeeping mode rec node s a v x =
      Bookkeeping (BK mode rec node s a v x)
   deriving (Functor, Applicative)

newtype
   EquationSystem mode rec node s a v =
      EquationSystem {runEquationSystem :: BK mode rec node s a v ()}

instance Monoid (EquationSystem mode rec node s a v) where
   mempty = EquationSystem $ return ()
   mappend (EquationSystem x) (EquationSystem y) =
      EquationSystem $ x >>! y



liftF ::
   (Sys.Value mode y, Record rec, Sum y) =>
   (x -> y) ->
   RecordExpression mode rec node s a v x ->
   RecordExpression mode rec node s a v y
liftF = liftA . SysRecord.lift1 . Expr.fromRule2 . Sys.assignment2

liftF2 ::
   (Sys.Value mode z, Record rec, Sum z) =>
   (x -> y -> z) ->
   RecordExpression mode rec node s a v x ->
   RecordExpression mode rec node s a v y ->
   RecordExpression mode rec node s a v z
liftF2 = liftA2 . SysRecord.lift2 . Expr.fromRule3 . Sys.assignment3


instance (Sum x) => Sum (Bookkeeping mode rec node s a v x) where
   (~+) = liftA2 (~+)
   (~-) = liftA2 (~-)
   negate = fmap Arith.negate

instance (Product x) => Product (Bookkeeping mode rec node s a v x) where
   (~*) = liftA2 (~*)
   (~/) = liftA2 (~/)
   recip = fmap Arith.recip
   constOne = fmap Arith.constOne

instance (Constant x) => Constant (Bookkeeping mode rec node s a v x) where
   zero = pure zero
   fromInteger  = pure . Arith.fromInteger
   fromRational = pure . Arith.fromRational

instance (Integrate x) => Integrate (Bookkeeping mode rec node s a v x) where
   type Scalar (Bookkeeping mode rec node s a v x) =
           Bookkeeping mode rec node s a v (Scalar x)
   integrate = fmap integrate



instance (Num x) => Num (Bookkeeping mode rec node s a v x) where
   fromInteger = pure . fromInteger

   (*) = liftA2 (*)
   (+) = liftA2 (+)
   (-) = liftA2 (-)

   abs = fmap abs
   signum = fmap signum


instance (Fractional x) => Fractional (Bookkeeping mode rec node s a v x) where
   fromRational = pure . fromRational
   (/) = liftA2 (/)

sqrt ::
   (Sys.Value mode x, Sum x, Floating x, Record rec) =>
   RecordExpression mode rec node s a v x ->
   RecordExpression mode rec node s a v x
sqrt = liftF P.sqrt


localVariable ::
   (Record rec, Verify.LocalVar mode a, Sum a) =>
   WriterT (System mode s) (ST s) (SysRecord.Variable mode rec s a)
localVariable = do
   vars <- lift $ sequenceA $ pure Verify.localVariable
   tell $ SysRecord.rules vars
   return vars

globalVariable ::
   (Record rec, Verify.GlobalVar mode a (Record.ToIndex rec) var node,
    Sum a, FormatValue (var node)) =>
   var node ->
   WriterT (System mode s) (ST s) (SysRecord.Variable mode rec s a)
globalVariable var = do
   vars <-
      lift $ for Record.indices $ \recIdx ->
         Verify.globalVariableDyn $ Idx.Record recIdx var
   tell $ SysRecord.rules vars
   return vars


infix 0 =.=, =%=

(=.=) ::
   (Sys.Value mode x) =>
   Expression mode rec node s a v x ->
   Expression mode rec node s a v x ->
   EquationSystem mode rec node s a v
(Bookkeeping xs) =.= (Bookkeeping ys) =
   EquationSystem $ lift . tell . System =<< liftM2 (=:=) xs ys

(=%=) ::
   (Sys.Value mode x, Record rec) =>
   RecordExpression mode rec node s a v x ->
   RecordExpression mode rec node s a v x ->
   EquationSystem mode rec node s a v
(Bookkeeping xs) =%= (Bookkeeping ys) =
   EquationSystem $ lift . tell =<< liftM2 SysRecord.equal xs ys


infix 0 =%%=, .=, %=, ?=

(=%%=) ::
   (Node.C node, SeqFlow.Lookup idx, Env.Element idx a v ~ x,
    Record rec, Sys.Value mode x) =>
   idx node -> idx node ->
   EquationSystem mode rec node s a v
x =%%= y  =  variableRecord x =%= variableRecord y

(.=) ::
   (Node.C node, SeqFlow.Lookup idx, Env.Element idx a v ~ x,
    Record rec, Sys.Value mode x) =>
   Record.Indexed rec (idx node) -> x ->
   EquationSystem mode rec node s a v
evar .= val  =  variable evar =.= constant val

(%=) ::
   (Node.C node, SeqFlow.Lookup idx, Env.Element idx a v ~ x,
    Record rec, Sys.Value mode x) =>
   idx node -> rec x ->
   EquationSystem mode rec node s a v
evar %= val  =  variableRecord evar =%= constantRecord val

(?=) ::
   (Node.C node, SeqFlow.Lookup idx, Env.Element idx a v ~ x,
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

join ::
   Bookkeeping mode rec node s a v (EquationSystem mode rec node s a v) ->
   EquationSystem mode rec node s a v
join (Bookkeeping m) =
   EquationSystem $ m >>= \(EquationSystem sys) -> sys


constant ::
   (Sys.Value mode x) =>
   x -> Expression mode rec node s a v x
constant = pure . Expr.constant

constantRecord ::
   (Sys.Value mode x, Record rec) =>
   rec x -> RecordExpression mode rec node s a v x
constantRecord = pure . Wrap . fmap Expr.constant


withLocalVar ::
   (Verify.LocalVar mode x, Sum x, Record rec) =>
   (RecordExpression mode rec node s a v x ->
    EquationSystem mode rec node s a v) ->
   EquationSystem mode rec node s a v
withLocalVar f = EquationSystem $ do
   v <- lift localVariable
   case f $ pure $ Wrap $ fmap Expr.fromVariable v of
      EquationSystem act -> act


newtype
   Lookup rec node s a v idx env =
      Lookup {
         getLookup ::
            (Env.Environment idx ~ env) =>
            idx node ->
            SeqFlow.Graph node
               (SysRecord.Variable mode rec s a)
               (SysRecord.Variable mode rec s v) ->
            Maybe
               (SysRecord.Variable mode rec s (Env.Element idx a v))
      }

lookup ::
   (SeqFlow.Lookup idx, Node.C node) =>
   idx node ->
   SeqFlow.Graph node
      (SysRecord.Variable mode rec s a)
      (SysRecord.Variable mode rec s v) ->
   Maybe
      (SysRecord.Variable mode rec s (Env.Element idx a v))
lookup =
   getLookup $
   Env.switchPart
      (Lookup $ SeqFlow.lookup)
      (Lookup $ SeqFlow.lookup)


variableRecord ::
   (Node.C node, SeqFlow.Lookup idx, Env.Element idx a v ~ x, Record rec) =>
   idx node -> RecordExpression mode rec node s a v x
variableRecord idx =
   Bookkeeping $
   MR.asks
      (maybe
         (error "EquationSystem.variableRecord: unknown variable")
         (Wrap . fmap Expr.fromVariable)
       .
       lookup idx)

variable ::
   (Node.C node, SeqFlow.Lookup idx, Env.Element idx a v ~ x, Record rec) =>
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
   EquationSystem mode rec node s a v
fromGraph equalInOutSums gv =
   case
      SeqFlow.mapGraph
         (pure . Wrap . fmap Expr.fromVariable)
         (pure . Wrap . fmap Expr.fromVariable) gv of
      g ->
         mconcat $
            foldMap
               (uncurry (fromTopology equalInOutSums) . snd)
               (SeqFlow.sequence g) :
            fromStorageSequences g :
            []

fromTopology ::
   (Verify.LocalVar mode a, Sum a, a ~ Scalar v,
    Verify.LocalVar mode v, Integrate v, Product v,
    Record rec, Node.C node) =>
   Bool ->
   RecordExpression mode rec node s a v v ->
   SeqFlow.Topology node
      (RecordExpression mode rec node s a v a)
      (RecordExpression mode rec node s a v v) ->
   EquationSystem mode rec node s a v
fromTopology equalInOutSums dtime topo =
   foldMap (fromEdge dtime) (Gr.edgeLabels topo)
   <>
   foldMap (fromSums equalInOutSums) (Gr.nodeLabels topo)
   <>
   foldMap
      (\(ins,ss,outs) ->
         (flip foldMap (SeqFlow.sumIn ss) $ \s ->
            splitStructEqs dtime (SeqFlow.flowSum s)
               SeqFlow.flowEnergyIn SeqFlow.flowXIn $ Map.elems ins)
         <>
         (flip foldMap (SeqFlow.sumOut ss) $ \s ->
            splitStructEqs dtime (SeqFlow.flowSum s)
               SeqFlow.flowEnergyOut SeqFlow.flowXOut $ Map.elems outs))
      (Gr.graphMap topo)

fromEdge ::
   (Sys.Value mode x, Product x, Record rec) =>
   RecordExpression mode rec node s a v x ->
   SeqFlow.Flow (RecordExpression mode rec node s a v x) ->
   EquationSystem mode rec node s a v
fromEdge dtime
      (SeqFlow.Flow {
         SeqFlow.flowEnergyOut = eout,
         SeqFlow.flowPowerOut = pout,
         SeqFlow.flowEnergyIn = ein,
         SeqFlow.flowPowerIn = pin,
         SeqFlow.flowEta = eta
      }) =
   (eout =%= dtime ~* pout) <>
   (ein  =%= dtime ~* pin)  <>
   (pout =%= eta ~* pin)


fromStorageSequences ::
   (Verify.LocalVar mode a, Constant a, Record rec, Node.C node) =>
   SeqFlow.Graph node
      (RecordExpression mode rec node s a v a)
      (RecordExpression mode rec node s a v v) ->
   EquationSystem mode rec node s a v
fromStorageSequences g =
   let stoutsum sec node =
          maybe (error "fromStorageSequences inStorages") id $
          SeqFlow.lookupStOutSum (Idx.ForNode (Idx.StOutSum sec) node) g
       stinsum sec node =
          maybe (error "fromStorageSequences outStorages") id $
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
    ra ~ RecordExpression mode rec node s a v a,
    rv ~ RecordExpression mode rec node s a v v) =>
   SeqFlow.Graph node ra rv ->
   node ->
   (ra, ra) ->
   Map Idx.Boundary ra ->
   EquationSystem mode rec node s a v
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
    ra ~ RecordExpression mode rec node s a v a) =>
   SeqFlow.Graph node ra rv ->
   node ->
   ra -> (Maybe Idx.Section, ra) ->
   EquationSystem mode rec node s a v
charge g node old (aug, now) =
   let sums =
          case aug of
             Nothing -> SeqFlow.Sums Nothing Nothing
             Just sec ->
                maybe (error "missing sum") id $
                SeqFlow.lookupSums (Idx.secNode sec node) g
   in  condSum now (SeqFlow.sumOut sums)
       =%=
       condSum old (SeqFlow.sumIn  sums)

condSum :: (Sum a) => a -> Maybe (SeqFlow.Sum a v) -> a
condSum x = maybe x (\s -> x ~+ SeqFlow.carrySum s)



fromSums ::
   (Verify.LocalVar mode a, Sum a, a ~ Scalar v,
    Verify.LocalVar mode v, Integrate v,
    Record rec,
    ra ~ RecordExpression mode rec node s a v a,
    rv ~ RecordExpression mode rec node s a v v) =>
   Bool ->
   SeqFlow.Sums ra rv ->
   EquationSystem mode rec node s a v
fromSums equalInOutSums s =
   let sumIn  = SeqFlow.sumIn s
       sumOut = SeqFlow.sumOut s
   in  (fold $
          guard equalInOutSums
          >>
          liftA2 equalSums sumIn sumOut)
       <>
       fromSum sumIn
       <>
       fromSum sumOut

equalSums ::
   (Sys.Value mode a, Sys.Value mode v, Record rec,
    ra ~ RecordExpression mode rec node s a v a,
    rv ~ RecordExpression mode rec node s a v v) =>
   SeqFlow.Sum ra rv ->
   SeqFlow.Sum ra rv ->
   EquationSystem mode rec node s a v
equalSums x y =
   (SeqFlow.flowSum x =%= SeqFlow.flowSum y)
   <>
   (SeqFlow.carrySum x =%= SeqFlow.carrySum y)

fromSum ::
   (Verify.LocalVar mode a, Sum a, a ~ Scalar v,
    Verify.LocalVar mode v, Integrate v,
    Record rec,
    ra ~ RecordExpression mode rec node s a v a,
    rv ~ RecordExpression mode rec node s a v v) =>
   Maybe (SeqFlow.Sum ra rv) ->
   EquationSystem mode rec node s a v
fromSum =
   foldMap $ \s -> SeqFlow.carrySum s =%= Arith.integrate (SeqFlow.flowSum s)


splitStructEqs ::
   (Verify.LocalVar mode x, Product x, Record rec,
    rx ~ RecordExpression mode rec node s a v x) =>
   rx ->
   rx ->
   (SeqFlow.Flow rx -> rx) ->
   (SeqFlow.Flow rx -> rx) ->
   [SeqFlow.Flow rx] ->
   EquationSystem mode rec node s a v
splitStructEqs dtime varsum energy xfactor =
   foldMap (splitFactors varsum energy (Arith.constOne dtime) xfactor)
   .
   NonEmpty.fetch


fromInStorages ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ RecordExpression mode rec node s a v x) =>
   rx -> [SeqFlow.Carry rx] ->
   EquationSystem mode rec node s a v
fromInStorages stoutsum outs =
   let maxEnergies = map SeqFlow.carryMaxEnergy outs
       stEnergies  = map SeqFlow.carryEnergy outs
   in  mconcat $
       splitStoreEqs stoutsum SeqFlow.carryEnergy SeqFlow.carryXOut outs :
       zipWith (=%=) maxEnergies
          (stoutsum : zipWith (~-) maxEnergies stEnergies)

fromOutStorages ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ RecordExpression mode rec node s a v x) =>
   rx -> [SeqFlow.Carry rx] ->
   EquationSystem mode rec node s a v
fromOutStorages stinsum ins =
   (withLocalVar $ \s ->
      splitStoreEqs s SeqFlow.carryMaxEnergy SeqFlow.carryXIn ins)
   <>
   splitStoreEqs stinsum SeqFlow.carryEnergy SeqFlow.carryXIn ins

splitStoreEqs ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ RecordExpression mode rec node s a v x) =>
   rx ->
   (SeqFlow.Carry rx -> rx) ->
   (SeqFlow.Carry rx -> rx) ->
   [SeqFlow.Carry rx] ->
   EquationSystem mode rec node s a v
splitStoreEqs varsum energy xfactor =
   foldMap (splitFactors varsum energy Arith.one xfactor)
   .
   NonEmpty.fetch

splitFactors ::
   (Verify.LocalVar mode x, Product x, Record rec,
    rx ~ RecordExpression mode rec node s a v x) =>
   rx -> (secnode -> rx) ->
   rx -> (secnode -> rx) ->
   NonEmpty.T [] secnode -> EquationSystem mode rec node s a v
splitFactors s ef one xf ns =
   (s =%= NonEmpty.foldl1 (~+) (fmap ef ns))
   <>
   (one =%= NonEmpty.foldl1 (~+) (fmap xf ns))
   <>
   (foldMap (\n -> ef n =%= s ~* xf n) ns)



variables ::
   (Node.C node, Record rec, Sum a, Sum v,
    Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeSectionScalar node,
    Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node) =>
   SeqFlowPlain.RangeGraph node ->
   WriterT (System mode s) (ST s)
      (SeqFlow.Graph node
         (SysRecord.Variable mode rec s a)
         (SysRecord.Variable mode rec s v))
variables =
   SeqFlow.traverseGraph id id
   .
   SeqFlow.mapGraphWithVar
      (\var () -> globalVariable var)
      (\var () -> globalVariable var)
   .
   SeqFlow.graphFromPlain

query ::
   (Traversable rec) =>
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
   SeqFlowPlain.RangeGraph node ->
   EquationSystem mode rec node s a v ->
   ST s
      (SeqFlow.Graph node
         (SysRecord.Variable mode rec s a)
         (SysRecord.Variable mode rec s v),
       Sys.T mode s ())
setup equalInOutSums gr sys = do
   (vars, System eqs) <-
      runWriterT $ do
         vars <- variables gr
         flip runReaderT vars $ runEquationSystem $
            fromGraph equalInOutSums vars <> sys
         return vars
   return (vars, eqs)

solveGen ::
   (Constant a, a ~ Scalar v,
    Product v, Integrate v,
    Record rec, Node.C node) =>
   Bool ->
   SeqFlowPlain.RangeGraph node ->
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
   SeqFlowPlain.RangeGraph node ->
   (forall s. EquationSystem Verify.Ignore rec node s a v) ->
   SeqFlow.Graph node (rec (Result a)) (rec (Result v))
solve = solveGen True

solveFromMeasurement ::
   (Constant a, a ~ Scalar v,
    Product v, Integrate v,
    Record rec, Node.C node) =>
   SeqFlowPlain.RangeGraph node ->
   (forall s. EquationSystem Verify.Ignore rec node s a v) ->
   SeqFlow.Graph node (rec (Result a)) (rec (Result v))
solveFromMeasurement = solveGen False

solveTracked ::
   (Verify.GlobalVar (Verify.Track output) a recIdx Var.ForNodeSectionScalar node,
    Constant a, a ~ Scalar v,
    Verify.GlobalVar (Verify.Track output) v recIdx Var.InSectionSignal node,
    Product v, Integrate v,
    Record rec, Record.ToIndex rec ~ recIdx, Node.C node) =>
   SeqFlowPlain.RangeGraph node ->
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
