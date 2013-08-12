{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EFA.Flow.EquationSystem where

import qualified EFA.Flow.Sequence.Quantity as SeqFlow

import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.SystemRecord as SysRecord
import EFA.Equation.SystemRecord (Expr, Record, Wrap(Wrap))

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph as Gr

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+),
           Product, (~*),
           Constant,
           Integrate, Scalar)

import EFA.Utility ((>>!))

import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.System as Sys


import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT, tell)

import Control.Monad.ST (ST)
import Control.Monad (guard)

import Control.Applicative (Applicative, pure, liftA2)

import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty

import Data.Traversable (Traversable, sequenceA)
import Data.Foldable (foldMap, fold)
import Data.Monoid (Monoid, (<>), mempty, mappend)


type Writer mode s = WriterT (SysRecord.System mode s) (ST s)

newtype System mode s = System {runSystem :: Writer mode s ()}

instance Monoid (System mode s) where
   mempty = System $ return ()
   mappend (System x) (System y)  =  System $ x >>! y


infix 0 =&=

(=&=) ::
   (Record rec, Sys.Value mode a) =>
   Expr mode rec s a ->
   Expr mode rec s a ->
   System mode s
x =&= y  =  System $ tell $ SysRecord.equal x y


localVariable ::
   (Record rec, Verify.LocalVar mode a, Sum a) =>
   Writer mode s (SysRecord.Variable mode rec s a)
localVariable = do
   vars <- lift $ sequenceA $ pure Verify.localVariable
   tell $ SysRecord.rules vars
   return vars

withLocalVar ::
   (Verify.LocalVar mode x, Sum x, Record rec) =>
   (Expr mode rec s x -> System mode s) ->
   System mode s
withLocalVar f = System $ do
   v <- localVariable
   case f $ Wrap $ fmap Expr.fromVariable v of
      System act -> act


fromTopology ::
   (Verify.LocalVar mode a, Sum a, a ~ Scalar v,
    Verify.LocalVar mode v, Integrate v, Product v,
    Record rec, Node.C node) =>
   Bool ->
   Expr mode rec s v ->
   SeqFlow.Topology node
      (Expr mode rec s a)
      (Expr mode rec s v) ->
   System mode s
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
   Expr mode rec s x ->
   SeqFlow.Flow (Expr mode rec s x) ->
   System mode s
fromEdge dtime
      (SeqFlow.Flow {
         SeqFlow.flowEnergyOut = eout,
         SeqFlow.flowPowerOut = pout,
         SeqFlow.flowEnergyIn = ein,
         SeqFlow.flowPowerIn = pin,
         SeqFlow.flowEta = eta
      }) =
   (eout =&= dtime ~* pout) <>
   (ein  =&= dtime ~* pin)  <>
   (pout =&= eta ~* pin)


fromSums ::
   (Verify.LocalVar mode a, Sum a, a ~ Scalar v,
    Verify.LocalVar mode v, Integrate v,
    Record rec,
    ra ~ Expr mode rec s a,
    rv ~ Expr mode rec s v) =>
   Bool ->
   SeqFlow.Sums ra rv ->
   System mode s
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
    ra ~ Expr mode rec s a,
    rv ~ Expr mode rec s v) =>
   SeqFlow.Sum ra rv ->
   SeqFlow.Sum ra rv ->
   System mode s
equalSums x y =
   (SeqFlow.flowSum x =&= SeqFlow.flowSum y)
   <>
   (SeqFlow.carrySum x =&= SeqFlow.carrySum y)

fromSum ::
   (Verify.LocalVar mode a, Sum a, a ~ Scalar v,
    Verify.LocalVar mode v, Integrate v,
    Record rec,
    ra ~ Expr mode rec s a,
    rv ~ Expr mode rec s v) =>
   Maybe (SeqFlow.Sum ra rv) ->
   System mode s
fromSum =
   foldMap $ \s -> SeqFlow.carrySum s =&= Arith.integrate (SeqFlow.flowSum s)

splitStructEqs ::
   (Verify.LocalVar mode x, Product x, Record rec,
    rx ~ Expr mode rec s x) =>
   rx ->
   rx ->
   (SeqFlow.Flow rx -> rx) ->
   (SeqFlow.Flow rx -> rx) ->
   [SeqFlow.Flow rx] ->
   System mode s
splitStructEqs dtime varsum energy xfactor =
   foldMap (splitFactors varsum energy (Arith.constOne dtime) xfactor)
   .
   NonEmpty.fetch


fromInStorages ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ SysRecord.Expr mode rec s x) =>
   rx -> [SeqFlow.Carry rx] ->
   System mode s
fromInStorages stoutsum outs =
   splitStoreEqs stoutsum SeqFlow.carryEnergy SeqFlow.carryXOut outs

fromOutStorages ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ SysRecord.Expr mode rec s x) =>
   rx -> [SeqFlow.Carry rx] ->
   System mode s
fromOutStorages stinsum ins =
   splitStoreEqs stinsum SeqFlow.carryEnergy SeqFlow.carryXIn ins

splitStoreEqs ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ Expr mode rec s x) =>
   rx ->
   (SeqFlow.Carry rx -> rx) ->
   (SeqFlow.Carry rx -> rx) ->
   [SeqFlow.Carry rx] ->
   System mode s
splitStoreEqs varsum energy xfactor =
   foldMap (splitFactors varsum energy Arith.one xfactor)
   .
   NonEmpty.fetch


splitFactors ::
   (Verify.LocalVar mode x, Product x, Record rec,
    rx ~ Expr mode rec s x) =>
   rx -> (secnode -> rx) ->
   rx -> (secnode -> rx) ->
   NonEmpty.T [] secnode -> System mode s
splitFactors s ef one xf ns =
   (s =&= NonEmpty.foldl1 (~+) (fmap ef ns))
   <>
   (one =&= NonEmpty.foldl1 (~+) (fmap xf ns))
   <>
   (foldMap (\n -> ef n =&= s ~* xf n) ns)
