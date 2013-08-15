{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EFA.Flow.EquationSystem where

import qualified EFA.Flow.Quantity as Quant

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.SystemRecord as SysRecord
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.SystemRecord (Expr, Record, Wrap(Wrap))
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, (~*), (~/),
           Constant, zero,
           Integrate, Scalar, integrate)

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph as Gr

import EFA.Report.FormatValue (FormatValue)

import EFA.Utility ((>>!))

import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.System as Sys
import UniqueLogic.ST.TF.Expression ((=:=))


import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Writer (WriterT, tell)

import Control.Monad.ST (ST)
import Control.Monad (guard)

import Control.Applicative (Applicative, pure, liftA, liftA2)

import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty

import Data.Traversable (Traversable, sequenceA, for)
import Data.Foldable (foldMap, fold)
import Data.Monoid (Monoid, (<>), mempty, mappend)

import qualified Prelude as P
import Prelude hiding (sqrt)


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


globalVariable ::
   (Record rec, Verify.GlobalVar mode a (Record.ToIndex rec) var node,
    Sum a, FormatValue (var node)) =>
   var node ->
   Writer mode s (SysRecord.Variable mode rec s a)
globalVariable var = do
   vars <-
      lift $ for Record.indices $ \recIdx ->
         Verify.globalVariableDyn $ Idx.Record recIdx var
   tell $ SysRecord.rules vars
   return vars


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
   Quant.Topology node
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
         (flip foldMap (Quant.sumIn ss) $ \s ->
            splitStructEqs dtime (Quant.flowSum s)
               Quant.flowEnergyIn Quant.flowXIn $ Map.elems ins)
         <>
         (flip foldMap (Quant.sumOut ss) $ \s ->
            splitStructEqs dtime (Quant.flowSum s)
               Quant.flowEnergyOut Quant.flowXOut $ Map.elems outs))
      (Gr.graphMap topo)

fromEdge ::
   (Sys.Value mode x, Product x, Record rec) =>
   Expr mode rec s x ->
   Quant.Flow (Expr mode rec s x) ->
   System mode s
fromEdge dtime
      (Quant.Flow {
         Quant.flowEnergyOut = eout,
         Quant.flowPowerOut = pout,
         Quant.flowEnergyIn = ein,
         Quant.flowPowerIn = pin,
         Quant.flowEta = eta
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
   Quant.Sums ra rv ->
   System mode s
fromSums equalInOutSums s =
   let sumIn  = Quant.sumIn s
       sumOut = Quant.sumOut s
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
   Quant.Sum ra rv ->
   Quant.Sum ra rv ->
   System mode s
equalSums x y =
   (Quant.flowSum x =&= Quant.flowSum y)
   <>
   (Quant.carrySum x =&= Quant.carrySum y)

fromSum ::
   (Verify.LocalVar mode a, Sum a, a ~ Scalar v,
    Verify.LocalVar mode v, Integrate v,
    Record rec,
    ra ~ Expr mode rec s a,
    rv ~ Expr mode rec s v) =>
   Maybe (Quant.Sum ra rv) ->
   System mode s
fromSum =
   foldMap $ \s -> Quant.carrySum s =&= Arith.integrate (Quant.flowSum s)

splitStructEqs ::
   (Verify.LocalVar mode x, Product x, Record rec,
    rx ~ Expr mode rec s x) =>
   rx ->
   rx ->
   (Quant.Flow rx -> rx) ->
   (Quant.Flow rx -> rx) ->
   [Quant.Flow rx] ->
   System mode s
splitStructEqs dtime varsum energy xfactor =
   foldMap (splitFactors varsum energy (Arith.constOne dtime) xfactor)
   .
   NonEmpty.fetch


fromInStorages ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ SysRecord.Expr mode rec s x,
    Quant.Carry carry) =>
   rx -> [carry rx] ->
   System mode s
fromInStorages stoutsum outs =
   splitStoreEqs stoutsum Quant.carryEnergy Quant.carryXOut outs

fromOutStorages ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ SysRecord.Expr mode rec s x,
    Quant.Carry carry) =>
   rx -> [carry rx] ->
   System mode s
fromOutStorages stinsum ins =
   splitStoreEqs stinsum Quant.carryEnergy Quant.carryXIn ins

splitStoreEqs ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ Expr mode rec s x,
    Quant.Carry carry) =>
   rx ->
   (carry rx -> rx) ->
   (carry rx -> rx) ->
   [carry rx] ->
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



type Ctx mode vars s = ReaderT vars (Writer mode s)

type
   Expression mode vars s x =
      Context mode vars s (Expr.T mode s x)

type
   RecordExpression mode vars rec s x =
      Context mode vars s (SysRecord.Expr mode rec s x)


newtype Context mode vars s x = Context (Ctx mode vars s x)
   deriving (Functor, Applicative)

newtype
   VariableSystem mode vars s =
      VariableSystem {runVariableSystem :: Ctx mode vars s ()}

instance Monoid (VariableSystem mode vars s) where
   mempty = VariableSystem $ return ()
   mappend (VariableSystem x) (VariableSystem y) =
      VariableSystem $ x >>! y



liftF ::
   (Sys.Value mode y, Record rec, Sum y) =>
   (x -> y) ->
   RecordExpression mode var rec s x ->
   RecordExpression mode var rec s y
liftF = liftA . SysRecord.lift1 . Expr.fromRule2 . Sys.assignment2

liftF2 ::
   (Sys.Value mode z, Record rec, Sum z) =>
   (x -> y -> z) ->
   RecordExpression mode var rec s x ->
   RecordExpression mode var rec s y ->
   RecordExpression mode var rec s z
liftF2 = liftA2 . SysRecord.lift2 . Expr.fromRule3 . Sys.assignment3


instance (Sum x) => Sum (Context mode vars s x) where
   (~+) = liftA2 (~+)
   (~-) = liftA2 (~-)
   negate = fmap Arith.negate

instance (Product x) => Product (Context mode vars s x) where
   (~*) = liftA2 (~*)
   (~/) = liftA2 (~/)
   recip = fmap Arith.recip
   constOne = fmap Arith.constOne

instance (Constant x) => Constant (Context mode vars s x) where
   zero = pure zero
   fromInteger  = pure . Arith.fromInteger
   fromRational = pure . Arith.fromRational

instance (Integrate x) => Integrate (Context mode vars s x) where
   type Scalar (Context mode vars s x) = Context mode vars s (Scalar x)
   integrate = fmap integrate



instance (Num x) => Num (Context mode vars s x) where
   fromInteger = pure . fromInteger

   (*) = liftA2 (*)
   (+) = liftA2 (+)
   (-) = liftA2 (-)

   abs = fmap abs
   signum = fmap signum


instance (Fractional x) => Fractional (Context mode vars s x) where
   fromRational = pure . fromRational
   (/) = liftA2 (/)

sqrt ::
   (Sys.Value mode x, Sum x, Floating x, Record rec) =>
   RecordExpression mode vars rec s x ->
   RecordExpression mode vars rec s x
sqrt = liftF P.sqrt


infix 0 =.=, =%=

(=.=) ::
   (Sys.Value mode x) =>
   Expression mode vars s x ->
   Expression mode vars s x ->
   VariableSystem mode vars s
(Context xs) =.= (Context ys) =
   VariableSystem $ lift . tell . SysRecord.System =<< liftA2 (=:=) xs ys

(=%=) ::
   (Sys.Value mode x, Record rec) =>
   RecordExpression mode vars rec s x ->
   RecordExpression mode vars rec s x ->
   VariableSystem mode vars s
(Context xs) =%= (Context ys) =
   VariableSystem $ lift . tell =<< liftA2 SysRecord.equal xs ys


join ::
   Context mode vars s (VariableSystem mode vars s) ->
   VariableSystem mode vars s
join (Context m) =
   VariableSystem $ m >>= \(VariableSystem sys) -> sys


constant ::
   (Sys.Value mode x) =>
   x -> Expression mode vars s x
constant = pure . Expr.constant

constantRecord ::
   (Sys.Value mode x, Record rec) =>
   rec x -> RecordExpression mode vars rec s x
constantRecord = pure . Wrap . fmap Expr.constant
