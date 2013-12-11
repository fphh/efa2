{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.Storage.EquationSystem where

import qualified EFA.Flow.Storage.Quantity as Quant
import qualified EFA.Flow.EquationSystem as EqSys
import EFA.Flow.EquationSystem (mixSumRules, mixFactorRules, mixLevelRules)

import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.SystemRecord (Record, Expr, MixLevel, Wrap(Wrap))
import EFA.Equation.Arithmetic (Sum, ZeroTestable, Constant)

import qualified EFA.Utility.FixedLength as FixedLength

import qualified UniqueLogic.ST.TF.Expression as Expr

import qualified Data.NonEmpty as NonEmpty
import Control.Applicative (pure)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Monoid, mempty, (<>))


data Options mode rec s a =
   Options {
      optOne :: Expr mode rec s a,
      optEqualFactorsOut, optEqualFactorsIn ::
         Expr mode rec s a ->
         EqSys.System mode s
   }

optionsDefault ::
   (Verify.LocalVar mode a, Constant a, ZeroTestable a, Record rec) =>
   Options mode rec s a
optionsDefault = optionsBase classOne

optionsBase ::
   (Verify.LocalVar mode a, Record rec) =>
   One mode rec s a ->
   Options mode rec s a
optionsBase (One one) =
   Options {
      optOne = one,
      optEqualFactorsOut = const mempty,
      optEqualFactorsIn = const mempty
   }

newtype One mode rec s a = One (Expr mode rec s a)

classOne ::
   (Verify.LocalVar mode a, Constant a, ZeroTestable a, Record rec) =>
   One mode rec s a
classOne = One Arith.one

customOne ::
   (Verify.LocalVar mode a, Record rec) =>
   a -> One mode rec s a
customOne = One . Wrap . pure . Expr.constant


sourceMix ::
   (Verify.LocalVar mode a, Sum a, Record rec) =>
   Options mode rec s a ->
   Options mode rec s a
sourceMix opts =
   opts {
      optEqualFactorsOut = mixFactorRules,
      optEqualFactorsIn = const mempty
   }

sinkMix ::
   (Verify.LocalVar mode a, Sum a, Record rec) =>
   Options mode rec s a ->
   Options mode rec s a
sinkMix opts =
   opts {
      optEqualFactorsOut = const mempty,
      optEqualFactorsIn = mixFactorRules
   }

mix ::
   (Verify.LocalVar mode a, Sum a, Record rec) =>
   MixLevel rec EqSys.MixOrientation ->
   Options mode rec s a ->
   Options mode rec s a
mix levels opts =
   opts {
      optEqualFactorsOut =
         mixLevelRules $ FixedLength.map (EqSys.Source==) levels,
      optEqualFactorsIn =
         mixLevelRules $ FixedLength.map (EqSys.Sink==) levels
   }


fromCarryEdges ::
   (Verify.LocalVar mode x, Sum x, Record rec,
    rx ~ Expr mode rec s x, Quant.Carry carry, Foldable f) =>
   Options mode rec s x ->
   f (carry rx) ->
   EqSys.System mode s
fromCarryEdges opts =
   foldMap $ \edge ->
      Quant.foldEnergy mixSumRules edge
      <>
      optEqualFactorsOut opts (Quant.carryXOut edge)
      <>
      optEqualFactorsIn opts (Quant.carryXIn edge)

fromInStorages ::
   (Verify.LocalVar mode x, Arith.Product x, ZeroTestable x, Record rec,
    rx ~ Expr mode rec s x, Quant.Carry carry) =>
   Options mode rec s x ->
   rx -> [carry rx] ->
   EqSys.System mode s
fromInStorages opts stoutsum outs =
   splitFactors opts stoutsum Quant.carryEnergy Quant.carryXOut outs

fromOutStorages ::
   (Verify.LocalVar mode x, Arith.Product x, ZeroTestable x, Record rec,
    rx ~ Expr mode rec s x, Quant.Carry carry) =>
   Options mode rec s x ->
   rx -> [carry rx] ->
   EqSys.System mode s
fromOutStorages opts stinsum ins =
   splitFactors opts stinsum Quant.carryEnergy Quant.carryXIn ins

splitFactors ::
   (Verify.LocalVar mode x, Arith.Product x, ZeroTestable x, Record rec,
    rx ~ Expr mode rec s x) =>
   Options mode rec s x ->
   rx ->
   (carry rx -> rx) ->
   (carry rx -> rx) ->
   [carry rx] ->
   EqSys.System mode s
splitFactors opts varsum energy xfactor =
   foldMap (EqSys.splitFactors varsum energy (optOne opts) xfactor)
   .
   NonEmpty.fetch
