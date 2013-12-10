{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.Storage.EquationSystem where

import qualified EFA.Flow.Storage.Quantity as Quant
import qualified EFA.Flow.EquationSystem as EqSys
import EFA.Flow.EquationSystem (mixSumRules, mixFactorRules, mixLevelRules)

import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.SystemRecord (Record, Expr, MixLevel)
import EFA.Equation.Arithmetic (Sum, ZeroTestable, Constant)

import qualified EFA.Utility.FixedLength as FixedLength

import qualified Data.NonEmpty as NonEmpty
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Monoid, mempty, (<>))


data Options mode rec s a =
   Options {
      optEqualFactorsOut, optEqualFactorsIn ::
         Expr mode rec s a ->
         EqSys.System mode s
   }

optionsDefault ::
   (Verify.LocalVar mode v, Record rec) =>
   Options mode rec s v
optionsDefault =
   Options {
      optEqualFactorsOut = const mempty,
      optEqualFactorsIn = const mempty
   }

sourceMix ::
   (Verify.LocalVar mode v, Sum v, Record rec) =>
   Options mode rec s v ->
   Options mode rec s v
sourceMix opts =
   opts {
      optEqualFactorsOut = mixFactorRules,
      optEqualFactorsIn = const mempty
   }

sinkMix ::
   (Verify.LocalVar mode v, Sum v, Record rec) =>
   Options mode rec s v ->
   Options mode rec s v
sinkMix opts =
   opts {
      optEqualFactorsOut = const mempty,
      optEqualFactorsIn = mixFactorRules
   }

mix ::
   (Verify.LocalVar mode v, Sum v, Record rec) =>
   MixLevel rec EqSys.MixOrientation ->
   Options mode rec s v ->
   Options mode rec s v
mix levels opts =
   opts {
      optEqualFactorsOut =
         mixLevelRules $ FixedLength.map (EqSys.Source==) levels,
      optEqualFactorsIn =
         mixLevelRules $ FixedLength.map (EqSys.Sink==) levels
   }


fromCarryEdges ::
   (Verify.LocalVar mode x, Constant x, Record rec,
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
   (Verify.LocalVar mode x, Constant x, ZeroTestable x, Record rec,
    rx ~ Expr mode rec s x, Quant.Carry carry) =>
   rx -> [carry rx] ->
   EqSys.System mode s
fromInStorages stoutsum outs =
   splitFactors stoutsum Quant.carryEnergy Quant.carryXOut outs

fromOutStorages ::
   (Verify.LocalVar mode x, Constant x, ZeroTestable x, Record rec,
    rx ~ Expr mode rec s x, Quant.Carry carry) =>
   rx -> [carry rx] ->
   EqSys.System mode s
fromOutStorages stinsum ins =
   splitFactors stinsum Quant.carryEnergy Quant.carryXIn ins

splitFactors ::
   (Verify.LocalVar mode x, Constant x, ZeroTestable x, Record rec,
    rx ~ Expr mode rec s x) =>
   rx ->
   (carry rx -> rx) ->
   (carry rx -> rx) ->
   [carry rx] ->
   EqSys.System mode s
splitFactors varsum energy xfactor =
   foldMap (EqSys.splitFactors varsum energy Arith.one xfactor)
   .
   NonEmpty.fetch
