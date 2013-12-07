{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.Storage.EquationSystem where

import qualified EFA.Flow.Storage.Quantity as Quant
import qualified EFA.Flow.EquationSystem as EqSys
import EFA.Flow.EquationSystem (mixSumRules, mixFactorRules)

import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.SystemRecord (Record, Expr)
import EFA.Equation.Arithmetic (Sum, Constant)

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

optionsSourceMix ::
   (Verify.LocalVar mode v, Sum v, Record rec) =>
   Options mode rec s v
optionsSourceMix =
   Options {
      optEqualFactorsOut = mixFactorRules,
      optEqualFactorsIn = const mempty
   }

optionsSinkMix ::
   (Verify.LocalVar mode v, Sum v, Record rec) =>
   Options mode rec s v
optionsSinkMix =
   Options {
      optEqualFactorsOut = const mempty,
      optEqualFactorsIn = mixFactorRules
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
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ Expr mode rec s x, Quant.Carry carry) =>
   rx -> [carry rx] ->
   EqSys.System mode s
fromInStorages stoutsum outs =
   splitFactors stoutsum Quant.carryEnergy Quant.carryXOut outs

fromOutStorages ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ Expr mode rec s x, Quant.Carry carry) =>
   rx -> [carry rx] ->
   EqSys.System mode s
fromOutStorages stinsum ins =
   splitFactors stinsum Quant.carryEnergy Quant.carryXIn ins

splitFactors ::
   (Verify.LocalVar mode x, Constant x, Record rec,
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
